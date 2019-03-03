calc :: String -> Float
calc = head . foldl f [] . words
  where
    f :: [Float] -> String -> [Float]
    f (x:y:zs) "+" = (y + x) : zs
    f (x:y:zs) "-" = (y - x) : zs
    f (x:y:zs) "*" = (y * x) : zs
    f (x:y:zs) "/" = (y / x) : zs
    f (x:y:zs) "FLIP" = y : x : zs
    f (x:zs) "ABS" = (abs x) : zs
    f xs y = read y : xs

--calc str = head (foldl f [] (words str))
--calc str = (head . foldl f [] . words) str
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort less ++ [x] ++ qsort more
  where
    less = filter (< x) xs
    more = filter (>= x) xs
