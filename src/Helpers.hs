module Helpers
  ( ignoreIrrelevantCharacters
  , getCurrentTimestamp
  , narrowTags
  , httpsClient
  , getNumber
  , getTitle
  , getPoints
  , getCommentsCount
  , makeArticle
  , tagToNumber
  , tagToTitle
  , tagToPoints
  , tagToCommentsCount
  , Article
  ) where

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Time
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.HTML.TagSoup

-- Getters
getNumber :: [a] -> a
getNumber tag = tag !! 0

getTitle :: [a] -> a
getTitle tag = tag !! 1

getPoints :: [a] -> a
getPoints tag = tag !! 2

getCommentsCount :: [a] -> a
getCommentsCount tag = tag !! 3

tagToNumber :: [Tag str] -> Maybe str
tagToNumber = maybeTagText . getNumber

tagToTitle :: [Tag str] -> Maybe str
tagToTitle = maybeTagText . getTitle

tagToPoints :: [Tag str] -> Maybe str
tagToPoints = maybeTagText . getPoints

tagToCommentsCount :: [Tag str] -> Maybe str
tagToCommentsCount = maybeTagText . getCommentsCount

-- Article data type
type Title = Maybe String

type Rank = Maybe String

type Score = Maybe String

type CommentCount = Maybe String

data Article =
  Article Title Rank Score CommentCount
  deriving (Show, Eq)

-- Make an Article.
makeArticle :: [Tag String] -> Article
makeArticle t =
  Article (tagToNumber t) (tagToTitle t) (tagToPoints t) (tagToCommentsCount t)

-- Narrow the HTML source to a Tag list that interests us.
narrowTags :: String -> [[Tag String]]
narrowTags src = do
  let tags = parseTags src
  partitions (~== ("<tr class=athing>" :: String)) tags

-- Function to not count certain special character.
ignoreIrrelevantCharacters :: String -> String
ignoreIrrelevantCharacters [] = []
ignoreIrrelevantCharacters (x:xs)
  | x == '.' || x == '_' || x == '-' = ignoreIrrelevantCharacters xs
  | otherwise = x : ignoreIrrelevantCharacters xs

-- Function to get the current timestamp as a formatted string
getCurrentTimestamp :: IO String
getCurrentTimestamp = do
  currentTime <- getCurrentTime
  let formattedTime =
        formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  return formattedTime

-- Return the html body from a http call to the Hacker News website.
httpsClient :: IO String
httpsClient = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest "https://news.ycombinator.com"
  response <- httpLbs request manager
  let body = responseBody response
  return $ LC.unpack body
