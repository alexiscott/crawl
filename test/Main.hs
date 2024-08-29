module Main where
import Helpers
import Test.HUnit
import qualified System.Exit as Exit

testString :: String
testString = "This is - a self-explained example"

test1 :: Test
test1 = TestCase (assertEqual "When counting words, consider only the spaced words and exclude any symbols." 5 (length (words(ignoreIrrelevantCharacters testString ))))

tests :: Test
tests = TestList [TestLabel "Do not count special symbols in the length of a phrase." test1]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
