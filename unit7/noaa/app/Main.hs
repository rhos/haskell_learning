module Main where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Char8    as LC
import           Network.HTTP.Simple

myToken :: BC.ByteString
myToken = "ERmftvSkiJVcupPQRAezZYvWMPtYtzkY"
noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"
apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

main :: IO ()
main = return ()

responseM :: IO (Response LC.ByteString)
responseM = httpLBS "http://news.ycombinator.com"

response :: IO Int
response = getResponseStatusCode <$> responseM