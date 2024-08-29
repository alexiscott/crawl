module Helpers (ignoreIrrelevantCharacters,
                getCurrentTimestamp) where

import Data.Time


-- Function to not count certain special character.
ignoreIrrelevantCharacters :: String -> String
ignoreIrrelevantCharacters [] = []
ignoreIrrelevantCharacters (x:xs) | x == '.' || x == '_' || x == '-'  = ignoreIrrelevantCharacters xs
                            | otherwise = x:ignoreIrrelevantCharacters xs


-- Function to get the current timestamp as a formatted string
getCurrentTimestamp :: IO String
getCurrentTimestamp = do
    currentTime <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    return formattedTime
