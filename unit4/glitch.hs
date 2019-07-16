import           System.Environment
import           Control.Monad
import qualified System.Random                 as RIO
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC

randomChar :: IO Char
randomChar = do
  randomInt <- RIO.randomRIO (0, 255)
  return (toEnum randomInt)

intToChar :: Int -> Char
intToChar int = toEnum safeInt where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc val bytes = mconcat [before, newChar, after]
 where
  (before, rest) = BC.splitAt loc bytes
  after          = BC.drop 1 rest
  newChar        = intToBC val

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- RIO.randomRIO (1, bytesLength)
  val      <- RIO.randomRIO (0, 255)
  return (replaceByte location val bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
 where
  (before, rest ) = BC.splitAt start bytes
  (target, after) = BC.splitAt size rest
  changed         = BC.reverse $ BC.sort target

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sSize   = 25
  let bLength = BC.length bytes
  start <- RIO.randomRIO (0, bLength - sSize)
  return $ sortSection start sSize bytes

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before, changed, after]
  where
  (before, rest ) = BC.splitAt start bytes
  (target, after) = BC.splitAt size rest
  changed         = BC.reverse target
  
randomReversBytes :: BC.ByteString -> IO BC.ByteString
randomReversBytes bytes = do
  let sSize   = 25
  let bLength = BC.length bytes
  start <- RIO.randomRIO (0, bLength - sSize)
  return $ reverseSection start sSize bytes

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
  [ randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  , randomReversBytes
  , randomSortSection
  , randomReplaceByte
  ]

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  imageFile <- BC.readFile filename
  -- glitched  <- randomReplaceByte imageFile
  -- glitched  <- randomSortSection imageFile
  glitched  <- foldM (\bytes func -> func bytes) imageFile glitchActions

  let glitchedFilename = mconcat ["g_", filename]
  BC.writeFile glitchedFilename glitched
  print "done"
