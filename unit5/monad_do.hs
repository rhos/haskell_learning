import qualified Data.Map                      as Map

maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM mpair = mpair >>= (\pair -> return $ max (fst pair) (snd pair))

maxPairM2 :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM2 mpair = (\pair -> max (fst pair) (snd pair)) <$> mpair

askForName :: IO ()
askForName = putStrLn "Name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloNameDo :: IO ()
helloNameDo = do
  askForName
  name <- getLine
  putStrLn $ nameStatement name

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main1 :: IO ()
main1 = do
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

main2 :: IO ()
main2 =
  getLine >>= (\name -> (\statement -> putStrLn statement) $ helloPerson name)

echo :: IO ()
echo = getLine >>= putStrLn

echo2 :: IO ()
echo2 = do
  val <- getLine
  putStrLn val

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)
data Candidate = Candidate
  { candidateId :: Int
  , codeReview :: Grade
  , cultureFit :: Grade
  , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
 where
  passedCoding     = codeReview candidate > B
  passedCultureFit = cultureFit candidate > C
  educationMin     = education candidate >= MS
  tests            = [passedCoding, passedCultureFit, educationMin]

testCandidate :: Candidate
testCandidate =
  Candidate { candidateId = 1, codeReview = A, cultureFit = A, education = PhD }

readInt :: IO Int
readInt = getLine >>= return . read

readGrade :: IO Grade
readGrade = getLine >>= return . read

readGrade2 :: IO Grade
readGrade2 = do
  val <- getLine
  let grade = read val
  return grade

readDegree :: IO Degree
readDegree = getLine >>= return . read

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "id:"
  id <- readInt
  putStrLn "grade:"
  grade <- readGrade
  putStrLn "fit:"
  fit <- readGrade
  putStrLn "degree:"
  degree <- readDegree
  return (Candidate id grade fit degree)

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

candidate1 :: Candidate
candidate1 = Candidate 1 A A BA

candidate2 :: Candidate
candidate2 = Candidate 2 C A PhD

candidate3 :: Candidate
candidate3 = Candidate 3 A B MS

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1, candidate1), (2, candidate2), (3, candidate3)]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

outOfMaybe :: Maybe String -> String
outOfMaybe Nothing  = "welp"
outOfMaybe (Just a) = a

candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed    = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

assessCandidates :: [Candidate] -> [String]
assessCandidates candidates = map (\x -> if x then "passed" else "failed")
                                  passed
  where passed = map viable candidates

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidate = do
  c <- candidate
  let passed    = viable c
  let statement = if passed then "passed" else "failed"
  return statement

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

type Pizza = (Double, Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costP1 < costP2
    then p1
    else p2
  where
    costP1 = costPerInch p1
    costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) =
  "The " ++
  show size ++
  " pizza " ++ "is cheaper at " ++ show costSqInch ++ " per square inch"
  where
    costSqInch = costPerInch (size, cost)

pizza :: IO ()
pizza = do
  putStrLn "What is the size of pizza 1"
  size1 <- getLine
  putStrLn "What is the cost of pizza 1"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2"
  size2 <- getLine
  putStrLn "What is the cost of pizza 2"
  cost2 <- getLine
  let pizza1      = (read size1, read cost1)
  let pizza2      = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)

pizza2 :: IO ()
pizza2 = 
  putStrLn "What is the size of pizza 1" >>
  getLine >>= 
    (\size1 ->
      putStrLn "What is the cost of pizza 1" >>
      getLine >>=
        (\cost1 ->
          putStrLn "What is the size of pizza 2" >>
          getLine >>=
            (\size2 ->
              putStrLn "What is the cost of pizza 2" >>
              getLine >>=
                (\cost2 -> 
                  (\pizza1 -> 
                    (\pizza2 ->
                      (\betterPizza ->
                        putStrLn (describePizza betterPizza)
                      ) $ comparePizzas pizza1 pizza2
                    ) (read size2, read cost2)
                  ) (read size1, read cost1)
                )
            )
        )
    )

costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]
    
sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1 = (size1,cost1)
  let pizza2 = (size2,cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

monadMain :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
monadMain s1 c1 s2 c2 = do
  size1 <- s1
  cost1 <- c1
  size2 <- s2
  cost2 <- c2
  let pizza1 = (size1,cost1)
  let pizza2 = (size2,cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
