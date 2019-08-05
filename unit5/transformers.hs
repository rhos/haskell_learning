import           Data.Char
import           Control.Monad
import           Control.Applicative
import           Control.Monad.Trans
import           System.IO

getPassphraseOld :: IO (Maybe String)
getPassphraseOld = do
  s <- getLine
  if isValid s then return $ Just s else return Nothing

-- The validation test could be anything we want it to be.

isValid :: String -> Bool
isValid s =
  length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

askPassphraseOld :: IO ()
askPassphraseOld = do
  putStrLn "Insert your new passphrase:"
  maybe_value <- getPassphraseOld
  case maybe_value of
    Just value -> putStrLn "Storing in database..."  -- do stuff
    Nothing    -> putStrLn "Passphrase invalid."

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
newtype State s a = State { runState :: s -> (s, a) }

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  (>>=) x f = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
      Nothing    -> return Nothing
      Just value -> runMaybeT $ f value

instance Monad m => Applicative (MaybeT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Functor (MaybeT m) where
  fmap = liftM

instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT $ return Nothing
  x <|> y = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
      Nothing -> runMaybeT y
      Just _  -> return maybe_value

instance Monad m => MonadPlus (MaybeT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans MaybeT where
  lift = MaybeT . (liftM Just)

getPassphrase :: MaybeT IO String
getPassphrase = do
  s <- lift getLine
  guard (isValid s) -- Alternative provides guard.
  return s

askPassphrase :: MaybeT IO ()
askPassphrase = do
  lift $ putStrLn "Insert your new passphrase:"
  value <- getPassphrase
  lift $ putStrLn "Storing in database..."

askPassphraseInf :: MaybeT IO ()
askPassphraseInf = do
  lift $ putStrLn "Insert your new passphrase:"
  value <- msum $ repeat getPassphrase
  lift $ putStrLn "Storing in database..."

main :: IO ()
main = do
  runMaybeT askPassphraseInf
  return ()

type Logger m = String -> m ()

noLogger :: (Monad m) => Logger m
noLogger _ = return ()

stderrLogger :: (MonadIO m) => Logger m
stderrLogger = liftIO . hPutStrLn stderr

fileLogger :: (MonadIO m) => FilePath -> Logger m
fileLogger logF x =
  liftIO . withFile logF AppendMode $ \h -> hPutStrLn h x

printFile :: (MonadIO m) => Logger m -> FilePath -> m ()
printFile log fp = do
    log ("Printing file: " ++ fp)
    liftIO (readFile fp >>= putStr)
    log "Done printing."