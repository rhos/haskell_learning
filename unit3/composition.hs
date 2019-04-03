import Data.Semigroup

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = fold . tobools
  where
    fold = foldr (||) False
    tobools = map f

instance Semigroup Integer where
  (<>) x y = x + y

data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | Transparent
  deriving (Show, Eq)

instance Semigroup Color where
  (<>) Transparent n = n
  (<>) n Transparent = n
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown

instance Monoid Color where
  mempty = Transparent
  mappend = (<>)

howMuch :: Int -> String
howMuch n
  | n > 10 = "much"
  | n > 0 = "less"
  | otherwise = "bs"

data Events =
  Events [String]

data Probs =
  Probs [Double]

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
  where
    combiner = (\x y -> mconcat [x, "-", y])

instance Semigroup Events where
  (<>) = combineEvents

instance Monoid Events where
  mappend = (<>)
  mempty = Events []

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Probs where
  mappend = (<>)
  mempty = Probs []

data PTable =
  PTable Events
         Probs

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events nProbs
  where
    tProbs = sum probs
    nProbs = (Probs . map (\x -> x / tProbs)) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where
      pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newE newP
    where
      newE = combineEvents e1 e2
      newP = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

coin :: PTable
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])
