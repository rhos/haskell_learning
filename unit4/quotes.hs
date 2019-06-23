quotes :: [String]
quotes = ["1", "1", "1", "1", "1"]


lookupQ :: [String] -> [String]
lookupQ [] = []
lookupQ ("n":xs) = []
lookupQ (x:xs) = q : (lookupQ xs)
  where q = quotes !! (read x - 1)


main :: IO()
main = do
  userInput <- getContents
  mapM_ putStrLn (lookupQ (lines userInput))
