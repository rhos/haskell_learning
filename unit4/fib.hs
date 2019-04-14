fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> [Int]
fib n = take n fibs

main :: IO ()
main = do
  putStrLn "n?:"
  number <- getLine
  let value = fib (read number)
  putStrLn (show value)
