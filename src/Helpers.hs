module Helpers (ignoreIrrelevantCharacters,
                getCurrentTimestamp, narrowTags, httpsClient) where

import Data.Time
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
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

-- Return the html body from a http call to the Hacker News website.
httpsClient :: IO String
httpsClient = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "https://news.ycombinator.com"
    response <- httpLbs request manager
    let body = responseBody response
    return $ LC.unpack body
