import qualified Data.Map                      as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList
  [ (1, "nYarlathoTep")
  , (2, "KINGinYELLOW")
  , (3, "dagon1997")
  , (4, "rcarter1919")
  , (5, "xCTHULHUx")
  , (6, "yogSOThoth")
  ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList
  [ ("nYarlathoTep", 2000)
  , ("KINGinYELLOW", 15000)
  , ("dagon1997"   , 300)
  , ("rcarter1919" , 12)
  , ("xCTHULHUx"   , 50000)
  , ("yogSOThoth"  , 150000)
  ]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits name = Map.lookup name creditsDB

creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits)
creditsFromIdStrange id = (pure lookupCredits) <*> (lookupUserName id)

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

echo :: IO ()
echo = getLine >>= putStrLn

echoVerbose :: IO ()
echoVerbose = putStrLn "we" >> getLine >>= putStrLn

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n * 2)

test380 :: IO ()
test380 = readInt >>= printDouble

askForName :: IO ()
askForName = putStrLn "Name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName =
  askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn

helloName2 :: IO ()
helloName2 = askForName >> nameStatement <$> getLine >>= putStrLn

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func ma = ma >>= (\a -> return (func a))

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mfunc ma = mfunc >>= (\func -> ma >>= (\a -> return $ func a))

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing    _    = Nothing
bind (Just val) func = func val
