import Data.Char

add3ToAll [] = []
add3ToAll (x:xs) = (3 + x) : add3ToAll xs

mul3ByAll [] = []
mul3ByAll (x:xs) = (3 * x) : mul3ByAll xs

myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter f [] = []
myFilter f (x:xs) =
  if f x
    then x : myFilter f xs
    else myFilter f xs

notf f = (\val -> not (f val))

myRemove f list = myFilter (notf f) list

myFolda f [x] = x
myFolda f (x:xs) = f x (myFolda f xs)

myFoldal f val list = myFolda f (val : list)

sumOfSquares xs = foldl (+) 0 (map (^ 2) xs)

rcons x y = y : x

myReverse xs = foldl rcons [] xs

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where
    newInit = f init x

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
  where
    rightResult = myFoldr f init xs

myElem val list = (length filtered) /= 0
  where
    filtered = filter (== val) list

isPalindrome sentence = processed == reverse processed
  where
    nosp = filter (/= ' ') sentence
    processed = map toLower nosp

myHarmonic n = foldl (+) 0 seq
  where
    seq = map (1 /) [1 .. n]

harmonic n = sum (take n seriesValues)
  where
    seriesPairs = zip (cycle [1.0]) [1.0,2.0 ..]
    seriesValues = map (\pair -> (fst pair) / (snd pair)) seriesPairs
