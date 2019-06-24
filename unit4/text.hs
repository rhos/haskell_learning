{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.IO

aWord :: T.Text
aWord = "Cheese"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

myLines :: T.Text -> [T.Text]
myLines text = T.splitOn "\n" text

myUnlines :: [T.Text] -> T.Text
myUnlines textLines = T.intercalate "\n" textLines

dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text
bgText =
  "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्। स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where
    pieces = T.splitOn query fullText
    highlighted = mconcat ["{", query, "}"]

main = do
  TIO.putStrLn (highlight dharma bgText)

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello ", name, "!"]

main1 :: IO ()
main1 = do
  TIO.putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement

toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines

main2 :: IO ()
main2 = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
  TIO.putStrLn ((T.pack . show . sum) numbers)
