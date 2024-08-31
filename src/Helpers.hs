module Helpers (ignoreIrrelevantCharacters,
                getCurrentTimestamp, narrowTags) where

import Data.Time
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.HTML.TagSoup

-- Narrow the HTML source to a Tag list that interests us.
narrowTags :: String -> [[Tag String]]
narrowTags src = do
  let tags = parseTags src
  partitions (~== ("<tr class=athing>" :: String)) tags

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

httpsClient :: IO ()
httpsClient = do
    manager <- newManager tlsManagerSettings
    let req = "https://news.ycombinator.com"
    response <- httpLbs req manager
    let htmlBody = responseBody response
    -- L.writeFile "newscombinator.html"  htmlBody -- It can be useful to have a local copy of the source.
    print htmlBody
