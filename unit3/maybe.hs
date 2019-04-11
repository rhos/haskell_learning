import qualified Data.List as List
import qualified Data.Map as Map

data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where
    getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ = length . filter (\x -> x == Just organ)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = List.intercalate ", " organList

numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just x) = x

data Container
  = Vat Organ
  | Cooler Organ
  | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location
  = Lab
  | Kitchen
  | Bathroom
  deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process = placeInLocation . organToContainer

report :: (Location, Container) -> String
report (location, container) =
  mconcat [(show container), " in the ", (show location)]

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = (report . process) organ
processAndReport Nothing = "error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where
    organ = Map.lookup id catalog

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers = length . filter (\x -> x == Nothing)

maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap f [] = []
maybeMap f (Just x:rest) = (Just (f x)) : (maybeMap f rest)
maybeMap f (Nothing:rest) = Nothing : (maybeMap f rest)