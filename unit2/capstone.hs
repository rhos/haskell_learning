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

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse (intToBits' n)
    missingBits = maxBits - (length reversedBits)
    leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
  where
    size = length bits
    indices = [size - 1,size - 2 .. 0]
    trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext =
  map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits plaintextBits)
  where
    padBits = map charToBits pad
    plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where
    bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data OneTimePad =
  OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

prngStream :: StreamCipher -> [Int]
prngStream (SC a b maxNumber seed) = rv : next
  where
    rv = prng a b maxNumber seed
    next = prngStream (SC a b maxNumber rv)

generatePad :: StreamCipher -> Int -> String
generatePad sc length = map toEnum ints
  where
    ints = take length (prngStream sc)

data StreamCipher =
  SC Int
     Int
     Int
     Int

instance Cipher StreamCipher where
  encode sc text = applyOTP pad text
    where
      pad = generatePad sc (length text)

  decode sc text = applyOTP pad text
    where
      pad = generatePad sc (length text)

mySC = SC 1337 7 100 12345
