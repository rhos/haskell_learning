{-# LANGUAGE NoImplicitPrelude #-}

module Palindrome
  ( isPalindrome
  )
where

import           Data.Text                      ( Text
                                                , filter
                                                , toLower
                                                , reverse
                                                )
import           Prelude                        ( Bool
                                                , not
                                                , (.)
                                                , (==)
                                                )
import           Data.Char                      ( isSpace
                                                , isPunctuation
                                                )

stripWhiteSpace :: Text -> Text
stripWhiteSpace text = filter (not . isSpace) text

stripPunctuation :: Text -> Text
stripPunctuation text = filter (not . isPunctuation) text

toLowerCase :: Text -> Text
toLowerCase text = toLower text

preProcess :: Text -> Text
preProcess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: Text -> Bool
isPalindrome text = cleanText == reverse cleanText
  where cleanText = preProcess text
