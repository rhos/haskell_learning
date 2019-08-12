module Main where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Char8    as LC
import           Network.HTTP.Simple
import           Data.Aeson
import           Data.Text                     as T
import           GHC.Generics
import           Control.Monad

myToken :: BC.ByteString
myToken = "ERmftvSkiJVcupPQRAezZYvWMPtYtzkY"
noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"
apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest
  :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method
    . setRequestHost host
    . setRequestHeader "token" [token]
    . setRequestPath path
    . setRequestSecure True
    . setRequestPort 443
    $ defaultRequest

buildRequestNOSSL
  :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNOSSL token host method path =
  setRequestMethod method
    $ setRequestHost host
    $ setRequestHeader "token" [token]
    $ setRequestSecure False
    $ setRequestPort 80
    $ setRequestPath path
    $ defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

data Book = Book
  { title :: T.Text
  , author :: T.Text
  , year :: Int
  } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book { author = "Will Kurt", title = "Learn Haskell", year = 2017 }
myBookJSON :: LC.ByteString
myBookJSON = encode myBook

rawJSON :: LC.ByteString
rawJSON =
  "{\"author\":\"Emil Ciroan\",\"title\": \"A Short History of Decay\",\"year=1949}"
bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

data ErrorMessage = ErrorMessage
  { message :: T.Text
  , errorCode :: Int
  } deriving Show

instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object ["message" .= message, "error" .= errorCode]

data NOAAResult = NOAAResult
  { uid :: T.Text
  , mindate :: T.Text
  , maxdate :: T.Text
  , name :: T.Text
  , datacoverage :: Float
  , resultId :: T.Text
  } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult
      <$> v
      .:  "uid"
      <*> v
      .:  "mindate"
      <*> v
      .:  "maxdate"
      <*> v
      .:  "name"
      <*> v
      .:  "datacoverage"
      <*> v
      .:  "id"

instance ToJSON NOAAResult where
  toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) =
    object  ["uid" .= uid
            ,"mindate" .= mindate
            ,"maxdate" .= maxdate
            ,"name" .= name
            ,"datacoverage" .= datacoverage
            ,"id" .= resultId]
instance ToJSON Resultset
instance ToJSON Metadata
instance ToJSON NOAAResponse

data Resultset = Resultset
  { offset :: Int
  , count :: Int
  , limit :: Int
  } deriving (Show,Generic)
instance FromJSON Resultset

data Metadata = Metadata
  {
  resultset :: Resultset
  } deriving (Show,Generic)
instance FromJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results :: [NOAAResult]
  } deriving (Show,Generic)
instance FromJSON NOAAResponse

printResults :: Either String [NOAAResult] -> IO ()
printResults (Left error)        = print error
printResults (Right results) = do
  forM_ results (print . name)

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
    then do
      print "saving request to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
      print "printing results"
      let noaaResponse = eitherDecode jsonBody :: Either String NOAAResponse
      let noaaResults = results <$> noaaResponse
      printResults noaaResults
    else print "request failed with error"

-- responseM :: IO (Response LC.ByteString)
-- responseM = httpLBS "http://news.ycombinator.com"

-- response :: IO Int
-- response = getResponseStatusCode <$> responseM
data IntList = EmptyList | Cons Int IntList deriving (Show,Generic)
instance ToJSON IntList
instance FromJSON IntList