data Die
  = S1
  | S2
  | S3
  | S4
  | S5
  | S6

instance Show Die where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

instance Eq Die where
  (==) S6 S6 = True
  (==) S5 S5 = True
  (==) S4 S4 = True
  (==) S3 S3 = True
  (==) S2 S2 = True
  (==) S1 S1 = True
  (==) _ _ = False

instance Ord Die where
  compare S6 S6 = EQ
  compare S6 _ = GT
  compare _ S6 = LT
  compare S5 S5 = EQ
  compare S5 _ = GT
  compare _ S5 = LT

instance Enum Die where
  toEnum 0 = S1
  toEnum 1 = S2
  toEnum 2 = S3
  toEnum 3 = S4
  toEnum 4 = S5
  toEnum 5 = S6
  toEnum _ = error "No such value"
  fromEnum S1 = 0
  fromEnum S2 = 1
  fromEnum S3 = 2
  fromEnum S4 = 3
  fromEnum S5 = 4
  fromEnum S6 = 5

data Number
  = One
  | Two
  | Three
  deriving (Show, Enum)

instance Eq Number where
  (==) num1 num2 = (fromEnum num1) == (fromEnum num2)

instance Ord Number where
  compare num1 num2 = compare (fromEnum num1) (fromEnum num2)

data FiveSidedDie
  = Side1
  | Side2
  | Side3
  | Side4
  | Side5
  deriving (Enum, Eq, Show)

class (Eq a, Enum a) =>
      Rollable a
  where
  roll :: Int -> a

instance Rollable FiveSidedDie where
  roll n = toEnum (n  `mod` 5)
