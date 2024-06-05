module Main (main) where

import Banjo
import Text.Read

handleOffsets :: [Int] -> IO ()
handleOffsets offsets = do
    putStrLn $ "Notes of chord: " ++ show (offsetsToNotes offsets) ++ "\n"
    let allFingerPlacements = rankChords $ filter isPlayable $ map banjoFrets $ allPossibleBanjoOffsetPositions offsets
    putStrLn "Input number of finger placements to show:"
    nPlacements <- getLine
    let chordsAsString = unlines $ map printBanjoChord $ take (read nPlacements::Int) allFingerPlacements
    let nFingerPlacements = length allFingerPlacements
    putStrLn $ "Found " ++ show nFingerPlacements ++ " finger placements, showing " ++ show (read nPlacements::Int) ++ ":\n" ++ chordsAsString



main :: IO ()
main = do
        putStrLn "Input chord name or list of notes:"
        line <- getLine
        if null line
          then return ()
        else if head line == ':' then do
          case readEither (drop 1 line)::Either String [Note] of
            Right validChord -> handleOffsets $ notesToOffsets validChord
            Left _ -> putStrLn ("Invalid list of notes: " ++ drop 1 line)
          main
        else do
          case parseChord line of
            Right validChord -> do
              putStrLn $ "Understood as: " ++ show validChord
              handleOffsets $ chordToListOfOffsets validChord
            Left err -> putStrLn err
          main
