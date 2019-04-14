import System.Random
import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

-- main :: IO()
-- main = do
  -- dieRoll <- randomRIO (minDie, maxDie)
  -- putStrLn (show dieRoll)

inputData :: Map.Map Int String
inputData = Map.fromList [(1,"Dummy")]

maybeMain :: Maybe String
maybeMain = do
  inputName <- Map.lookup 1 inputData
  let statement = helloPerson inputName
  return statement