module Banjo (module Banjo) where

import Data.List (nub, sortBy, permutations, isPrefixOf, subsequences, (\\))
import Data.Either (fromRight)
import Text.Read

-- Data structure definitions

data BaseNote = C | D | E | F | G | A | B deriving (Show, Read)
data Accidental = Natural | Sharp | Flat
data Note = Note BaseNote Accidental
data MainMarking = Major | Minor | Dim | Aug | Sus4 | Sus2 deriving (Show)
data Alternation =  Minor7 | Major7 | Minor9 | Major9 | Add9  deriving (Show, Eq, Ord)
data Chord = Chord Note MainMarking [Alternation]
data Parsed a = Parsed a Int -- Type so functions can return length of parsed sequence

instance Show Accidental where
  show Natural = ""
  show Sharp = "#"
  show Flat = "b"

instance Show Note where
  show (Note bass acc) = show bass ++ show acc


instance Show Chord where
  show (Chord n m s) = (show n) ++ " " ++ (show m) ++ showAlternations s
                       where
                           showAlternations [] = ""
                           showAlternations (x:xs) = " " ++ show x ++ showAlternations xs

instance Read Note where
  readsPrec _ input =
    case input of
      (base:accidental:r) -> case readEither [base] of
        Right baseNote -> case accidental of
          'b' -> [(Note baseNote Flat, r)]
          '#' -> [(Note baseNote Sharp, r)]
          _   -> []
        Left _ -> []
      (base:r) -> case readEither [base] of
        Right baseNote -> [(Note baseNote Natural, r)]
        Left _ -> []
      _ -> []


  readList str = case words str of
      [] -> []
      xs -> do
              let notes = map readEither xs::[(Either String Note)]
              if any isLeft notes then
                []
              else
                [(map (fromRight (error "Unexpexted Left")) notes, "")]
                  where
                    isLeft :: Either a b -> Bool
                    isLeft (Left _) = True
                    isLeft _        = False

instance Enum Note where
  fromEnum (Note C Natural) = 0
  fromEnum (Note C Sharp) = 1
  fromEnum (Note D Flat) = 1
  fromEnum (Note D Natural) = 2
  fromEnum (Note D Sharp) = 3
  fromEnum (Note E Flat) = 3
  fromEnum (Note E Natural) = 4
  fromEnum (Note E Sharp) = 5
  fromEnum (Note F Flat) = 4
  fromEnum (Note F Natural) = 5
  fromEnum (Note F Sharp) = 6
  fromEnum (Note G Flat) = 6
  fromEnum (Note G Natural) = 7
  fromEnum (Note G Sharp) = 8
  fromEnum (Note A Flat) = 8
  fromEnum (Note A Natural) = 9
  fromEnum (Note A Sharp) = 10
  fromEnum (Note B Flat) = 10
  fromEnum (Note B Natural) = 11
  fromEnum (Note B Sharp) = 0
  fromEnum (Note C Flat) = 11

  toEnum 0  = Note C Natural
  toEnum 1  = Note C Sharp
  toEnum 2  = Note D Natural
  toEnum 3  = Note D Sharp
  toEnum 4  = Note E Natural
  toEnum 5  = Note F Natural
  toEnum 6  = Note F Sharp
  toEnum 7  = Note G Natural
  toEnum 8  = Note G Sharp
  toEnum 9  = Note A Natural
  toEnum 10 = Note A Sharp
  toEnum 11 = Note B Natural
  toEnum n  = toEnum (n `mod` 12)


-- Banjo specific functions

banjoStrings :: [Int]
banjoStrings = [2, 7, 11, 2] -- D G H D

banjoFrets :: [Int] -> [Int]
banjoFrets = stringFrets banjoStrings

banjoNFrets :: Int
banjoNFrets = 22


-- Given a list of instrument string offsets and note offsets calculate the frets for each string
stringFrets :: [Int] -> [Int] -> [Int]
stringFrets instrumentStrings notes = zipWith (-) notes instrumentStrings

--------------------- PARSING FUNCTIONS -----------------------

---- Parses a chord given as string "Am" to the Chord data type "Chord (Note A Natural) Minor []"
parseChord :: String -> Either String Chord
parseChord str = do
                    Parsed rootNote lenNote <- parseNote str
                    Parsed mainMarking lenMarking <- parseMainMarking $ drop lenNote str
                    alternations <- parseAlternations $ drop (lenNote + lenMarking) str
                    Right (Chord rootNote mainMarking alternations)

-- Given a chord as string try to parse first 1/2 characters as a Note
parseNote :: String -> Either String (Parsed Note)
parseNote [] = Left "No chord input."
parseNote [base] = case readEither [base]::Either String BaseNote of
                     Left _ -> Left ("Invalid note: " ++ [base])
                     Right note -> Right (Parsed (Note note Natural) 1)
parseNote (base:accidental:_) = case readEither [base]::Either String BaseNote of
                                  Left _ -> Left ("Invalid note: " ++ [base])
                                  Right note ->
                                      if accidental == 'b' then
                                        Right (Parsed (Note note Flat) 2)
                                      else if accidental == '#' then
                                        Right (Parsed (Note note Sharp) 2)
                                      else
                                        Right (Parsed (Note note Natural) 1)


-- Given a string try to parse the MainMarking as a prefix of the string, if no MainMarking is found the chord is Major
parseMainMarking :: String -> Either String (Parsed MainMarking)
parseMainMarking str | (isPrefixOf "maj7" str) = Right (Parsed Major 0)
                     | isPrefixOf "maj9" str   = Right (Parsed Major 0)
                     | (isPrefixOf "m" str)    = Right (Parsed Minor 1)
                     | (isPrefixOf "dim" str)  = Right (Parsed Dim 3)
                     | (isPrefixOf "aug" str)  = Right (Parsed Aug 3)
                     | (isPrefixOf "sus4" str) = Right (Parsed Sus4 4)
                     | (isPrefixOf "sus2" str) = Right (Parsed Sus2 4)
                     | otherwise               = Right (Parsed Major 0)

-- Given a string try to parse it into a list of chord alternations
parseAlternations :: String -> Either String [Alternation]
parseAlternations [] = Right []
parseAlternations str
  | isPrefixOf "maj7" str = do
      rest <- parseAlternations (drop 4 str)
      Right (Major7:rest)
  | isPrefixOf "7" str = do
      rest <- parseAlternations (drop 1 str)
      Right (Minor7:rest)
  | isPrefixOf "9" str = do
      rest <- parseAlternations (drop 1 str)
      Right (Minor9:rest)
  | isPrefixOf "maj9" str = do
      rest <- parseAlternations (drop 4 str)
      Right (Major9:rest)
  | isPrefixOf "add9" str = do
      rest <- parseAlternations (drop 4 str)
      Right (Add9:rest)
  | otherwise = Left ("Invalid alternation for chord: " ++ str)


----------------- FUNCTIONS FOR CREATING FINGER PLACEMENTS -------------------

-- Converts the Chord data type to a list of notes represented as offsets
chordToListOfOffsets :: Chord -> [Int]
chordToListOfOffsets (Chord n m a) = (rootOffset:(map (`mod` 12) $ map (+rootOffset) (mainMarkingOffsets ++ altOffsets)))
                                    where
                                      altOffsets = alternationsToOffsets a []
                                      mainMarkingOffsets = markingToOffsets m
                                      rootOffset = fromEnum n


-- Converts the alternations of chord to list of offsets RELATIVE TO THE ROOT NOTE
alternationsToOffsets :: [Alternation] -> [Int] -> [Int]
alternationsToOffsets [] acc = acc
alternationsToOffsets (x:xs) [] = alternationsToOffsets xs (alternationToOffsets x [])
alternationsToOffsets (x:xs) acc = alternationsToOffsets xs (alternationToOffsets x acc)

-- Defines what alternation adds what notes RELATIVE TO THE ROOT NOTE
alternationToOffsets :: Alternation -> [Int] -> [Int]
alternationToOffsets Minor7 offs = (10:offs)
alternationToOffsets Major7 offs = (11:offs)
alternationToOffsets Minor9 offs = (10:14:offs)
alternationToOffsets Major9 offs = (11:14:offs)
alternationToOffsets Add9 offs = (14:offs)

-- Defines what MainMarking adds what notes RELATIVE TO THE ROOT NOTE
markingToOffsets :: MainMarking -> [Int]
markingToOffsets Sus4 = [5, 7]
markingToOffsets Sus2 = [2, 7]
markingToOffsets Major = [4, 7]
markingToOffsets Minor = [3, 7]
markingToOffsets Dim = [3, 6]
markingToOffsets Aug = [4, 8]

-- From a given list of notes as offsets creates a list of lists of all the possible permutations and octave changes
allPossibleBanjoOffsetPositions :: [Int] -> [[Int]]
allPossibleBanjoOffsetPositions off = nub $ concat [permutations x | x <- possibleOffsets]
                                      where
                                        possibleOffsets = nub $ concat [createOctaves x | x <- (fillChordBanjo off)]

-- If the chord consists of < 4 notes we duplicate some of the notes. This creates all the possible combinations
fillChordBanjo :: [Int] -> [[Int]]
fillChordBanjo off | l == 3 = nub [(off!!i:off) | i <- [0..2]::[Int]]
                   | l == 2 = nub [(first:second:off), (first:first:off), (second:second:off)]
                   | l == 1 = [[x | x <- off, _ <- [0..3]::[Int]]]
                   | l == 4 = [off]
                   | otherwise = [] -- Anything with more than 4 notes is not playable on the banjo
                      where
                        l = length off
                        first = off!!0
                        second = off!!1

-- Each note of the chord could also be played octaves higher. This function creates all the possible combinations
createOctaves :: [Int] -> [[Int]]
createOctaves s = nub [addOctaves x | x <- threeWaySplits]
                  where
                    addOctaves (n, one, two) = n ++ (map (+12) one) ++ (map (+24) two)
                    threeWaySplits = [(a,b,(s \\ (a ++ b))) | a <- (subsequences s), b <- (subsequences (s \\ a))]

-- Checks if the list of fret positions is playable (no negative frets and no frets higher than maximum)
isPlayable :: [Int] -> Bool
isPlayable off | not (isNegative off || highFret off) = True
               | otherwise = False
                  where
                     isNegative [] = False
                     isNegative (x:xs) = x < 0 || isNegative xs
                     highFret [] = False
                     highFret (x:xs) = x > banjoNFrets || highFret xs

-- Ranks the finger placements by score (the chord is scored on the relative distances between frets and
-- the absolute value of the frets, higher frets mean worse score)
rankChords :: [[Int]] -> [[Int]]
rankChords offsets = sortBy (\l r -> compare (penalty l) (penalty r)) offsets
                  where
                    penalty off = (sum [abs (x - y) | x <- off, y <- off]) + sum off


----------------- PRINTING FUNCTIONS -----------------

-- Creates a string of a finger placement
printBanjoChord :: [Int] -> String
printBanjoChord chord = concat [(show (toEnum (banjoStrings!!i)::Note)) ++ " |---" ++ (show f) ++ "---|\n" | (f, i) <- reverse (zip chord [0..3])]

-- Converts a list of offsets to a list of Notes
offsetsToNotes :: [Int] -> [Note]
offsetsToNotes [] = []
offsetsToNotes (x:xs) = (toEnum x: offsetsToNotes xs)

notesToOffsets :: [Note] -> [Int]
notesToOffsets [] = []
notesToOffsets (x:xs) = (fromEnum x: notesToOffsets xs)
