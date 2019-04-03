import qualified Data.Map as Map

data Box a =
  Box a
  deriving (Show)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

boxMap :: (a -> b) -> [Box a] -> [Box b]
boxMap f [] = []
boxMap f ((Box x):rest) = (Box mapped) : (boxMap f rest)
  where
    mapped = f x

data Triple a =
  Triple a
         a
         a
  deriving (Show)

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> b) -> Triple a -> Triple b
transform f (Triple x y z) = Triple (f x) (f y) (f z)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 52.2 12.3

data List a
  = Empty
  | Cons a
         (List a)
  deriving (Show)

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest) = Cons (func a) (ourMap func rest)

data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

values :: [Organ]
values = map snd (Map.toList organCatalog)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Int]
organCounts = map countOrgan allOrgans
  where
    countOrgan = (\organ -> (length . filter (== organ)) values)

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)