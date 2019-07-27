import qualified Data.Map                      as Map
import           System.Environment
data RobotPart = RobotPart  { name :: String
                            , description :: String
                            , cost :: Double
                            , count :: Int
                            } deriving Show

leftArm :: RobotPart
leftArm = RobotPart { name        = "left arm"
                    , description = "left arm for face punching!"
                    , cost        = 1000.00
                    , count       = 3
                    }
rightArm :: RobotPart
rightArm = RobotPart { name        = "right arm"
                     , description = "right arm for kind hand gestures"
                     , cost        = 1025.00
                     , count       = 5
                     }
robotHead :: RobotPart
robotHead = RobotPart { name        = "robot head"
                      , description = "this head looks mad"
                      , cost        = 5092.25
                      , count       = 2
                      }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat
  [ "<h2>"
  , partName
  , "</h2>"
  , "<p><h3>desc</h3>"
  , partDesc
  , "</p><p><h3>cost</h3>"
  , partCost
  , "</p><p><h3>count</h3>"
  , partCount
  , "</p>"
  ]
 where
  partName  = name part
  partDesc  = description part
  partCost  = show (cost part)
  partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
 where
  keys    = [1, 2, 3]
  vals    = [leftArm, rightArm, robotHead]
  keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

data Box a =
  Box a
  deriving (Show)

instance Functor Box where
  fmap func (Box v) = Box (func v)

morePresents :: Int -> Box a -> Box [a]
morePresents n box = replicate n <$> box

boxA :: Box Int
boxA = Box 1

boxInBox :: Box (Box Int)
boxInBox = fmap (\x -> Box x) boxA

unwrap :: Box a -> a
unwrap (Box x) = x

printCost :: Maybe Double -> IO ()
printCost Nothing     = putStrLn "item not found"
printCost (Just cost) = print cost

main :: IO ()
main = do
  id <- getArgs
  let part = Map.lookup (read $ head id) partsDB
  printCost (cost <$> part)

primes :: [Integer]
primes = sieve [2 ..]
  where sieve (p : xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]


main2 :: IO ()
main2 = do
  putStrLn "enter a part number 1"
  partNo1 <- getLine
  putStrLn "enter a part number 2"
  partNo2 <- getLine
  let part1    = Map.lookup (read partNo1) partsDB
  let part2    = Map.lookup (read partNo2) partsDB
  let cheapest = min <$> (cost <$> part1) <*> (cost <$> part2)
  printCost cheapest
