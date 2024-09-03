module Helpers
  ( ignoreIrrelevantCharacters
  , allArticles
  , findCommentsCount
  , findNumber
  , findPoints
  , findTitle
  , getCommentsCount
  , getCurrentTimestamp
  , getNumber
  , getPoints
  , getTitle
  , httpsClient
  , sortArticlesByComments
  , makeArticle
  , lessThanOrEqual5Words
  , moreThan5Words
  , narrowTags
  , numericCount
  , printArticles
  , sortArticlesByPoints
  , tagToCommentsCount
  , tagToNumber
  , tagToPoints
  , tagToTitle
  , Article(Article)
  ) where

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe
import Data.Time
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.HTML.TagSoup
import Text.Read (readMaybe)

decodeBody :: BL.ByteString -> String
decodeBody body = TL.unpack $ TLE.decodeUtf8 body

-- Presenting Articles.
printArticle :: Article -> String
printArticle (Article mRank mTitle mPoints mComments) =
  "Rank: " ++ rankStr ++ "\n" ++
  "Title: " ++ titleStr ++ "\n" ++
  "Points count: " ++ pointsStr ++
  "Comment count: " ++ commentsStr ++ "\n\n"
  where
    rankStr     | Just rank <- mRank     = rank
                | otherwise              = "Rank not available"

    titleStr    | Just title <- mTitle   = title
                | otherwise              = "Title not available"

    pointsStr   | Just points <- mPoints = show points ++ " points\n"
                | otherwise              = "No points\n"

    commentsStr | Just comments <- mComments = show comments ++ " comments\n\n"
                | otherwise                   = "No comments yet\n\n"

printArticles :: [Article] -> String
printArticles articles = unlines $ map printArticle articles

-- Retrieve all Articles from the HMTL source that is provided.
allArticles :: [[Tag String]] -> [Article]
allArticles tags = map makeArticle tags

-- Functions for getting the relevant parts out of a Tag String from an individual Article.
findNumber :: [Tag String] -> Maybe String
findNumber tags = do
  partition <-
    listToMaybe $ partitions (~== ("<span class=rank>" :: String)) tags
  secondTag <- listToMaybe $ drop 1 . take 2 $ partition
  maybeTagText secondTag

findTitle :: [Tag String] -> Maybe String
findTitle tags = do
  partition <-
    listToMaybe $ partitions (~== ("<span class=titleline>" :: String)) tags
  secondTag <- listToMaybe $ drop 2 $ partition
  maybeTagText secondTag

findPoints :: [Tag String] -> Maybe String
findPoints tags = do
  partition <-
    listToMaybe $ partitions (~== ("<span class=score>" :: String)) tags
  secondTag <- listToMaybe $ drop 1 . take 2 $ partition
  maybeTagText secondTag

findCommentsCount :: [Tag String] -> Maybe String
findCommentsCount tags = do
  partition <-
    listToMaybe $ partitions (~== ("<span class=age>" :: String)) tags
  secondTag <- listToMaybe $ drop 14 $ partition
  maybeTagText secondTag

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

type Score = Maybe Int

type CommentCount = Maybe Int

data Article =
  Article Title Rank Score CommentCount
  deriving (Show, Eq, Ord)

-- Make an Article.
makeArticle :: [Tag String] -> Article
makeArticle tags =
  Article
    (findNumber tags)
    (findTitle tags)
    (numericCount (findPoints tags))
    (numericCount (findCommentsCount tags))

numericCount :: Maybe String -> Maybe Int
numericCount (Just c) =
    case words c of
        (x:_) -> readMaybe x  -- Try to read the first word as an Int.
        [] -> Nothing  -- Handle the case where the string is empty.
numericCount Nothing = Nothing

-- Narrow the HTML source to a Tag list that interests us.
narrowTags :: String -> [[Tag String]]
narrowTags src = do
  let tags = parseTags src
  partitions (~== ("<tr class=athing>" :: String)) tags

moreThan5Words :: [Article] -> [Article]
moreThan5Words = filter (\(Article _ (Just title) _ _) -> length (words (ignoreIrrelevantCharacters title)) > 5)

lessThanOrEqual5Words :: [Article] -> [Article]
lessThanOrEqual5Words = filter (\(Article _ (Just title) _ _) -> length (words (ignoreIrrelevantCharacters title)) <= 5)


getCommentsCountFromArticle :: Article -> Int
getCommentsCountFromArticle (Article _ _ _ (Just commentsCount)) = commentsCount
getCommentsCountFromArticle (Article _ _ _ Nothing) = 0

getPointsCountFromArticle :: Article -> Int
getPointsCountFromArticle (Article _ _ (Just pointsCount) _) = pointsCount
getPointsCountFromArticle (Article _ _ Nothing _) = 0

sortArticlesByComments :: [Article] -> [Article]
sortArticlesByComments [] = []
sortArticlesByComments (p:xs) =
    sortArticlesByComments lesser ++ [p] ++ sortArticlesByComments greater
  where
    count = getCommentsCountFromArticle p
    lesser  = filter (\x -> getCommentsCountFromArticle x <= count) xs
    greater = filter (\x -> getCommentsCountFromArticle x > count) xs

sortArticlesByPoints :: [Article] -> [Article]
sortArticlesByPoints [] = []
sortArticlesByPoints (p:xs) =
    sortArticlesByPoints lesser ++ [p] ++ sortArticlesByPoints greater
  where
    count = getPointsCountFromArticle p
    lesser  = filter (\x -> getPointsCountFromArticle x <= count) xs
    greater = filter (\x -> getPointsCountFromArticle x > count) xs


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
  return $ decodeBody body
