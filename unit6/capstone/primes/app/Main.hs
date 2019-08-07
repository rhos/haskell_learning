module Main where

import           Primes

main :: IO ()
main = do
  inVal <- getLine
  let res = isPrime $ read inVal
  case res of
    Nothing  -> putStrLn "nope"
    Just val -> putStrLn $ show val
