data Book = Book
  { author :: Creator
  , isbn :: String
  , bookTitle :: String
  , bookYear :: Int
  , bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double
  }

data CollectibleToy = CollectibleToy
  { name :: String
  , descrption :: String
  , toyPrice :: Double
  }

data AuthorName = AuthorName
  { firstName :: String
  , lastName :: String
  }

--data SportsCar =
--  SportsCar Car
--            Spoiler
type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName
         LastName
  | NameWithMiddle FirstName
                   MiddleName
                   LastName
  | TwoInitialsWithLast Char
                        Char
                        LastName

data Author =
  Author Name

data Artist
  = Person Name
  | Band String

data Creator
  = AuthorCreator Author
  | ArtistCreator Artist

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

--madeBy :: StoreItem -> String
--madeBy (BookItem book) = show (author book)
--madeBy (RecordItem record) = show (artist record)
--madeBy _ = "unknown"
data Pamphlet = Pamphlet
  { pamphletTitle :: String
  , description :: String
  , contact :: String
  }

data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

type Radius = Double

type Height = Double

type Width = Double

data Shape
  = Circle Radius
  | Square Height
  | Rectangle Height
              Width
  deriving (Show)

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square h) = 4 * h
perimeter (Rectangle h w) = 2 * h + 2 * w

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Square h) = h ^ 2
area (Rectangle h w) = h * w
