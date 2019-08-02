import           Control.Monad
import           GHC.Base
import           Data.Char

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return $ 2 ^ value

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  val <- [1 .. n]
  return (2 ^ val, 3 ^ val)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2, 4 .. n]
  oddValue  <- [1, 3 .. n]
  return (evenValue, oddValue)

allEvenOdds2 :: Int -> [(Int, Int)]
allEvenOdds2 n =
  [2, 4 .. n]
    >>= (\evenValue ->
          [1, 3 .. n] >>= (\oddValue -> return (evenValue, oddValue))
        )

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard (even value)
  return value

evensGuard2 :: Int -> [Int]
evensGuard2 n = [1 .. n] >>= (\value -> guard (even value) >> return value)

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = do
  guard (y /= 0)
  return (x `div` y)

-- >> is done via >>=, so empty :: Maybe Int is Nothing and >>= for Nothing _ is Nothing
-- pure () is Just (), so we can discard that in >> and continue sequencing
-- something like (>>) discard next = discard >>= \_ -> next
safeDiv2 :: Int -> Int -> Maybe Int
safeDiv2 x y = guard (y /= 0) >> return (x `div` y)

evenSquares :: [Int]
evenSquares = do
  n <- [0 .. 9]
  let nSquared = n ^ 2
  guard $ even nSquared
  return nSquared

powersOfTwo2 :: Int -> [Int]
powersOfTwo2 n = [ 2 ^ value | value <- [1 .. n] ]

powersOfTwoAndThree2 :: Int -> [(Int, Int)]
powersOfTwoAndThree2 n = [ (2 ^ val, 3 ^ val) | val <- [1 .. n] ]

powersOfTwoAndThree3 :: Int -> [(Int, Int)]
powersOfTwoAndThree3 n =
  [ (twos, threes)
  | val <- [1 .. n]
  , let twos   = 2 ^ val
  , let threes = 3 ^ val
  ]

allEvenOdds3 :: Int -> [(Int, Int)]
allEvenOdds3 n =
  [ (evenValue, oddValue) | evenValue <- [2, 4 .. n], oddValue <- [1, 3 .. n] ]

evensGuard3 :: Int -> [Int]
evensGuard3 n = [ value | value <- [1 .. n], even value ]

answer :: [String]
answer =
  [ "Mr. " ++ capVal
  | val <- ["brown", "blue", "pink", "organge", "white"]
  , let capVal = (\(x : xs) -> toUpper x : xs) val
  ]

monthEnds :: [Int]
monthEnds = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int] -> [Int]
dates ends = [ date | end <- ends, date <- [1 .. end] ]

datesDo :: [Int] -> [Int]
datesDo ends = do
  end  <- ends
  date <- [1 .. end]
  return date

datesMonad :: [Int] -> [Int]
datesMonad ends =
  ends >>= 
    (\end->
      [1 .. end] >>=
        (\date ->
          return date
        )
    )