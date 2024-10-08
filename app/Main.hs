module Main where

import Helpers
  ( allArticles
  , getCurrentTimestamp
  , httpsClient
  , lessThanOrEqual5Words
  , moreThan5Words
  , narrowTags
  , printArticles
  , sortArticlesByComments
  , sortArticlesByPoints
  )

main :: IO ()
main = do
  html <- httpsClient
  let tags = narrowTags html
  currentTimeStamp <- getCurrentTimestamp
  putStrLn
    "To see a list of filtered articles with long titles and sorted by the greatest number of comments, enter 1.\nFor shorter headlines and sorted by points, enter 2.\n"
  result <- getLine
  let userNum = read result :: Int
  let action
        | userNum == 1 =
          putStrLn .
          printArticles . reverse . sortArticlesByComments . moreThan5Words $
          allArticles tags
        | userNum == 2 =
          putStrLn .
          printArticles . reverse . sortArticlesByPoints . lessThanOrEqual5Words $
          allArticles tags
        | otherwise =
          putStrLn "Please try again, entering either the number 1 or 2."
  action
  -- Write the request timestamp to a file.
  appendFile "scraper-log.txt" $
    "The user's filter selection was: " ++
    show userNum ++ ", at " ++ currentTimeStamp ++ "\n"
