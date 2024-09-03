module Main where

import Helpers (getCurrentTimestamp,
                allArticles,
                lessThanOrEqual5Words,
                moreThan5Words,
                httpsClient,
                printArticles,
                narrowTags)

main :: IO ()
main = do
  html <- httpsClient
  let tags = narrowTags html
  currentTimeStamp <- getCurrentTimestamp
  putStrLn "To see a list of filtered articles, \nenter 1 for long headlines, 2 for short ones."
  result <- getLine
  let userNum = read result :: Int
  let action | userNum == 1 = putStrLn $ printArticles $ moreThan5Words $ allArticles tags
             | userNum == 2 = putStrLn $ printArticles $ lessThanOrEqual5Words $ allArticles tags
             | otherwise = putStrLn "Please try again, entering either the number 1 or 2."
  action
  -- Write the request timestamp to a file.
  appendFile "scraper-log.txt" $ "The user's filter selection was: " ++ show userNum ++ ", at " ++ currentTimeStamp  ++ "\n"
