module Main where

import Text.HTML.TagSoup
import Helpers (getCurrentTimestamp,
                ignoreIrrelevantCharacters,
                httpsClient,
                narrowTags)


main :: IO [[Tag String]]
main = do
  -- scrapedArticles <- scrapeURL "https://news.ycombinator.com" articles
  -- currentTimeStamp <- getCurrentTimestamp
  -- putStrLn "To see a list of filtered articles, \nenter 1 for long headlines, 2 for short ones."
  -- result <- getLine
  -- let userNum = read result :: Int
  -- let action | userNum == 1 = printArticles (moreThan5Words scrapedArticles)
  --            | userNum == 2 = printArticles (lessThanOrEqual5Words scrapedArticles)
  --            | otherwise = putStrLn "Please try again, entering either the number 1 or 2."
  -- action
  -- -- Write the request timestamp to a file.
  -- appendFile "scraper-log.txt" $ "The user's filter selection was: " ++ show userNum ++ ", at " ++ currentTimeStamp  ++ "\n"
  html <- httpsClient
  let tags = narrowTags html
  return tags


-- main' :: [Tag String]
-- main' = do
--   html <- httpsClient
--   let tags = narrowTags html
--   putStrLn tags



type Title = String
type Rank = String

data Article =
     Article Title Rank
      deriving (Show, Eq)
