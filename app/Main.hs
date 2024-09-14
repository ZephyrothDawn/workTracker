{-# LANGUAGE RecordWildCards #-}
module WorkTracker where

import Data.Time
import Data.List

-- Data types
data WorkItem = WorkItem
    { description :: String
    , duration :: NominalDiffTime
    , date :: Day
    } deriving (Show, Eq)

type WorkLog = [WorkItem]

-- Functions
addWork :: WorkLog -> String -> NominalDiffTime -> IO WorkLog
addWork log desc dur = do
    today <- getCurrentTime
    let workItem = WorkItem desc dur (utctDay today)
    return (workItem : log)

totalDuration :: WorkLog -> NominalDiffTime
totalDuration = sum . map duration

displayWorkLog :: WorkLog -> String
displayWorkLog log = unlines $
    ["Work Log:"] ++
    [show date ++ ": " ++ description ++ " (" ++ showDuration duration ++ ")"
    | WorkItem {..} <- reverse log]
    ++ ["Total time: " ++ showDuration (totalDuration log)]

showDuration :: NominalDiffTime -> String
showDuration d =
    let (hours, remainingSeconds) = properFraction (d / 3600) :: (Integer, NominalDiffTime)
        (minutes, _) = properFraction (remainingSeconds / 60) :: (Integer, NominalDiffTime)
    in show hours ++ "h " ++ show minutes ++ "m"

-- Main program
main :: IO ()
main = do
    putStrLn "Work Tracker"
    loop []
  where
    loop :: WorkLog -> IO ()
    loop log = do
        putStrLn "\nEnter a command (add/view/quit):"
        cmd <- getLine
        case cmd of
            "add" -> do
                putStrLn "Enter work description:"
                desc <- getLine
                putStrLn "Enter duration in minutes:"
                durStr <- getLine
                let dur = fromIntegral (read durStr :: Int) * 60
                newLog <- addWork log desc dur
                putStrLn "Work item added."
                loop newLog
            "view" -> do
                putStrLn (displayWorkLog log)
                loop log
            "quit" -> putStrLn "Goodbye!"
            _ -> do
                putStrLn "Invalid command. Try again."
                loop log