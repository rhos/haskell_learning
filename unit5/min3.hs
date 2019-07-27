minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree a b c = min a $ min b c

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
  putStrLn "3 nums"
  minInt <- minOfInts
  putStrLn $ show minInt ++ " is the smallest"

minOfMaybies :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
minOfMaybies a b c = minOfThree <$> a <*> b <*> c