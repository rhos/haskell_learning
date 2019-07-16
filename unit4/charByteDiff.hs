import           System.IO
import           System.Environment
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  inChars <- B.readFile filename

  let bytes = B.length inChars
  putStrLn "Bytes:"
  print bytes

  let chars = T.length $ E.decodeUtf8 inChars
  putStrLn "Characters:"
  print chars

  putStrLn "Diff:"
  print $ bytes - chars
