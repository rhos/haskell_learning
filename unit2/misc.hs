calc :: String -> Float
calc = head . foldl f [] . words
--calc str = head (foldl f [] (words str))
--calc str = (head . foldl f [] . words) str
  where
    f :: [Float] -> String -> [Float]
    f (x:y:zs) "+" = (y + x) : zs
    f (x:y:zs) "-" = (y - x) : zs
    f (x:y:zs) "*" = (y * x) : zs
    f (x:y:zs) "/" = (y / x) : zs
    f (x:y:zs) "FLIP" = y : x : zs
    f (x:zs) "ABS" = (abs x) : zs
    f xs y = read y : xs

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
