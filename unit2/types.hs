x :: Int
x = 2

y :: Integer
y = 2

simple :: a -> a
simple x = x

streetAddress :: (Int, String)
streetAddress = (123, "Depressive St.")

halve :: Integer -> Integer
halve val = div val 2

printDouble :: Int -> String
printDouble val = show (val * 2)

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

addressTest = (((makeAddress 1) "Sad St") "Truth Town")

makeAddressLambdaA = (\number -> (\street -> (\town -> (number, street, town))))

addressTestA = (((makeAddressLambdaA 2) "Dead St") "Truth Town")

makeAddressLambdaB number street town =
  (\number -> (\street -> (\town -> (number, street, town)) town) street) number

addressTestB = (((makeAddressLambdaB 3) "Rotten St") "Truth Town")

myAverage :: (Integral a, Fractional b) => [a] -> b
myAverage list = fromIntegral (sum list) / fromIntegral (length list)

--alias
type FirstName = String

type LastName = String

type MiddleName = String

type PatientName = (String, String)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

patientInfo :: PatientName -> Int -> Int -> String
patientInfo patientName age height = name ++ " " ++ ageHeight
  where
    name = firstName patientName ++ ", " ++ lastName patientName
    ageHeight = show age ++ " " ++ show height

data Sex
  = Male
  | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType
  = Pos
  | Neg

data ABOType
  = A
  | B
  | AB
  | O

data BloodType =
  BloodType ABOType
            RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

-- pattern matching on values instance was constructed with!!
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

data Name
  = Name FirstName
         LastName
  | NameWithMiddle FirstName
                   MiddleName
                   LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

-- data Patient =
--   Patient Name
--           Sex
--           Int
--           Int
--           Int
--           BloodType
data Patient = Patient
  { name :: Name
  , sex :: Sex
  , age :: Int
  , height :: Int
  , weight :: Int
  , bloodType :: BloodType
  }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: Patient
janeESmith =
  Patient
    (NameWithMiddle "Jane" "Elizabeth" "Smith")
    Female
    28
    62
    140
    (BloodType B Neg)

jackieSmith :: Patient
jackieSmith =
  Patient
    { name = Name "Jackie" "Smith"
    , age = 43
    , sex = Female
    , height = 62
    , weight = 115
    , bloodType = BloodType O Neg
    }

olderJackie = jackieSmith {age = 44}

patientsSameAge :: Patient -> Patient -> Bool
patientsSameAge p1 p2 = age p1 == age p2

addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

class Describable a where
  describe :: a -> String

data Icecream
  = Chocolate
  | Vanilla
  deriving (Show, Eq, Ord)

inc :: Int -> Int
inc x = x + 1

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n == maxBound
    then minBound
    else succ n
