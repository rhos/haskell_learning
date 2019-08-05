module Main where
import           GHC.List

head :: Monoid a => [a] -> a
head (x : _) = x
head []      = mempty

main :: IO()
main = return ()