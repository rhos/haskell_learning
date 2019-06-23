import Control.Monad
import System.Environment

-- main :: IO()
-- main = do
--     vals <- mapM (\_ -> getLine) [1..3]
--     mapM_ putStrLn vals
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let linesToRead =
--         if length args > 0
--           then read (head args)
--           else 0 :: Int
--   numbers <- replicateM linesToRead getLine
--   let ints = map read numbers
--   print $ sum ints
-- myReplicateM :: Monad m => Int -> m a -> m [a]
-- myReplicateM n func = mapM (\_ -> func) [1 .. n]
reverser :: IO ()
reverser = do
  input <- getContents
  let reversed = reverse input
  putStrLn reversed

sampleData = ['6', '2', '\n', '2', '1', '\n']

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  let squares = map (^2) numbers
  print (sum (take 5 squares))
