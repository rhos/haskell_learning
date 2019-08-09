module Main where
import           Data.Char

myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

eitherHead :: [a] -> Either String a
eitherHead []      = Left "There is no head because the list is empty"
eitherHead (x : _) = Right x

primes :: [Int]
primes = [2, 3, 5, 7]

maxN :: Int
maxN = 10

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge     = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

displayResult :: Either PrimeError Bool -> String
displayResult (Right True      ) = "It's prime"
displayResult (Right False     ) = "It's composite"
displayResult (Left  primeError) = show primeError

isPrime :: Int -> Either PrimeError Bool
isPrime n | n < 2     = Left InvalidValue
          | n > maxN  = Left TooLarge
          | otherwise = Right (n `elem` primes)

main :: IO ()
main = do
  putStrLn "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  putStrLn (displayResult result)

allDigits :: String -> Bool
allDigits val = all (== True) (map isDigit val)

addStrInts :: String -> String -> Either Int String
addStrInts val1 val2
  | allDigits val1 && allDigits val2       = Left (read val1 + read val2)
  | not (allDigits val1 || allDigits val2) = Right "both args invalid"
  | not (allDigits val1)                   = Right "first arg invalid"
  | otherwise                              = Right "second arg invalid"

alleq :: Eq a => [a] -> Maybe a -> Bool
alleq []      _       = True
alleq (h : t) Nothing = alleq t (Just h)
alleq (h : t) (Just e) | h == e    = alleq t (Just e)
                       | otherwise = False

safeSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a
safeSucc n = if n == maxBound then Nothing else Just (succ n)

safeTail :: [a] -> [a]
safeTail []       = []
safeTail (_ : xs) = xs

safeLast :: [a] -> Either a String
safeLast [] = Right "empty list"
safeLast xs = safeLast' 10000 xs

safeLast' :: Int -> [a] -> Either a String
safeLast' 0 _        = Right "List exceeds safe bound"
safeLast' _ (x : []) = Left x
safeLast' n (x : xs) = safeLast' (n - 1) xs
