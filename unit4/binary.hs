{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

sampleBytes :: BC.ByteString
sampleBytes = "6"

bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack