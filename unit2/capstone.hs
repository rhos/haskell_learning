data Alphabet
  = L1
  | L2
  | L3
  | L4
  deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN size c = toEnum rotation
  where
    half = size `div` 2
    offset = fromEnum c + half
    rotation = offset `mod` size

rotChar :: Char -> Char
rotChar char = rotN size char
  where
    size = 1 + fromEnum (maxBound :: Char)

rotEncode :: [Alphabet] -> [Alphabet]
rotEncode vals = map rotSize vals
  where
    size = 1 + fromEnum (maxBound :: Alphabet)
    rotSize = rotN size

xorBool :: Bool -> Bool -> Bool
xorBool val1 val2 = (val1 || val2) && (not (val1 && val2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if (remainder == 0)
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2
