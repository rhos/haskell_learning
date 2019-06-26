import System.IO

main :: IO()
main = do
  f <- openFile "some_text.txt" ReadMode
  fline <- hGetLine f
  putStrLn fline
  sline <- hGetLine f
  o <- openFile "out.txt" WriteMode
  hPutStrLn o sline
  hClose o
  hClose f
  putStrLn "done"