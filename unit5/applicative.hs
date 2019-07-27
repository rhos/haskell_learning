import qualified Data.Map                      as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList
  [ ("Arkham"   , (42.6054, -70.7829))
  , ("Innsmouth", (42.8250, -70.8150))
  , ("Carcosa"  , (29.9714, -90.7694))
  , ("New York" , (40.7776, -73.9691))
  ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
 where
  rlat  = toRadians lat
  rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
 where
  (rlat1, rlong1) = latLongToRads coords1
  (rlat2, rlong2) = latLongToRads coords2
  dlat            = rlat2 - rlat1
  dlong           = rlong2 - rlong1
  a = (sin (dlat / 2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlong / 2)) ^ 2
  c               = 2 * atan2 (sqrt a) (sqrt (1 - a))
  earthRadius     = 3961.0

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO ioVal1 ioVal2 = do
  val1 <- ioVal1
  val2 <- ioVal2
  let dist = haversine val1 val2
  return dist

haversineIO2 :: IO LatLong -> IO LatLong -> IO Double
haversineIO2 ioVal1 ioVal2 = haversine <$> ioVal1 <*> ioVal2

printDistance :: Maybe Double -> IO ()
printDistance Nothing         = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just a) (Just b) = Just $ a + b
addMaybe _        _        = Nothing

maybeInc :: Maybe (Integer -> Integer)
maybeInc = (+) <$> Just 1

maybeInc5 :: Maybe Integer
maybeInc5 = maybeInc <*> Just 5

maybeIncNothing :: Maybe Integer
maybeIncNothing = maybeInc <*> Nothing

val1 = Just 10
val2 = Just 5
r1 = (*) <$> val1 <*> val2
r2 = div <$> val1 <*> val2
r3 = mod <$> val1 <*> val2

main :: IO ()
main = do
  putStrLn "Start"
  start <- getLine
  let startCity = Map.lookup start locationDB
  putStrLn "End"
  end <- getLine
  let endCity  = Map.lookup end locationDB
  let distance = haversine <$> startCity <*> endCity
  printDistance distance

data User = User
  { name :: String
  , gamerId :: Int
  , score :: Int
  } deriving Show

serverUsername :: Maybe String
serverUsername = Just "Sue"
serverGamerId :: Maybe Int
serverGamerId = Just 1337
serverScore :: Maybe Int
serverScore = Just 9001

serverUser :: Maybe User
serverUser = User <$> serverUsername <*> serverGamerId <*> serverScore