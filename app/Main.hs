module Main (main) where

import Banjo

main :: IO ()
main = do
        putStrLn "Input chord name:"
        line <- getLine
        if null line
          then return ()
          else do
            let parsedChord = parseChord line
            let chordAsOffsets = chordToListOfOffsets parsedChord
            let allFingerPlacements = rankChords $ filter isPlayable $ map banjoFrets $ allPossibleBanjoOffsetPositions chordAsOffsets
            putStrLn $ "Understood as: " ++ show parsedChord
            putStrLn $ "Notes of chord: " ++ show (offsetsToNotes chordAsOffsets) ++ "\n"
            putStrLn "Input number of finger placements to show:"
            nPlacements <- getLine
            let chordsAsString = printChords (read nPlacements::Int) allFingerPlacements
            let nFingerPlacements = length allFingerPlacements
            putStrLn $ "Found " ++ show nFingerPlacements ++ " finger placements, showing " ++ show (read nPlacements::Int) ++ ":\n" ++ chordsAsString
            main

