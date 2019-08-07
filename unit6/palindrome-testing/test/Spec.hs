-- import           Lib
-- import           Data.Char
-- import           Test.QuickCheck

-- assert :: Bool -> String -> String -> IO ()
-- assert test passStatement failStatement =
--   if test then putStrLn passStatement else putStrLn failStatement

-- prop_punctuationInvariant text = preprocess text == preprocess noPuncText
--   where noPuncText = filter (not . isPunctuation) text

-- main :: IO ()
-- main = do
--   putStrLn "Running tests..."
--   --assert (isPalindrome "racecar")     "passed 'racecar'"  "FAIL: 'racecar'"
--   --assert (isPalindrome "racecar!")    "passed 'racecar!'" "FAIL: 'racecar!'"
--   --assert ((not . isPalindrome) "cat") "passed 'cat'"      "FAIL: 'cat'"
--   --assert (isPalindrome "racecar.")    "passed 'racecar.'" "FAIL: 'racecar.'"
--   --quickCheck prop_punctuationInvariant
--   quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
--   putStrLn "done!"

import           Lib
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Data.Char                      ( isPunctuation )
import           Data.Text                     as T

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  putStrLn "done!"
