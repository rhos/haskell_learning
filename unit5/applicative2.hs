test359 :: Maybe String -> Maybe String -> Maybe String
test359 a b = (++) <$> a <*> b
test360 :: IO String
test360 = pure "Test"
orderTest :: Maybe Int
orderTest = (<*>) (pure (6 +)) (Just 6)
orderTest2 :: Maybe Int
orderTest2 = pure (6 +) <*> Just 6
listContext :: [Int] -> [Int] -> [Int]
listContext a b = (+) <$> a <*> b

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 2000]

totalPrize :: [Int]
totalPrize = (+) <$> doorPrize <*> boxPrize

primes :: Integer -> [Integer]
primes n = filter isNotComposite twoToN
 where
  twoToN         = [2 .. n]
  composite      = (*) <$> twoToN <*> twoToN
  isNotComposite = not . (`elem` composite)

data User = User
    { name :: String
    , gamerId :: Int
    , score :: Int
    } deriving Show

testNames :: [String]
testNames =
  [ "John Smith"
  , "Robert'); DROP TABLE Students;--"
  , "Christina NULL"
  , "Randall Munroe"
  ]

testIds :: [Int]
testIds = [1337, 0123, 9999]

testScores :: [Int]
testScores = [0, 1000, -99]

testData :: [User]
testData = User <$> testNames <*> testIds <*> testScores

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func a = pure func <*> a

example :: Maybe Int
example = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

beer :: [Int]
beer = [6,12]

leftBeer :: [Int]
leftBeer = (+ (-4)) <$> beer

friends :: [Int]
friends = [2,3]

drinkers :: [Int]
drinkers = (+2) <$> friends

toDrink :: [Int]
toDrink = [3,4]

result :: [Int]
result = pure (-) <*> leftBeer <*> (pure (*) <*> toDrink <*> drinkers)

