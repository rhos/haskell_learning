respond phrase =
  if '!' `elem` phrase
    then "wow!"
    else "nope!"

takeLast n aList = reverse (take n (reverse aList))

ones n = take n (cycle [1])

myRepeat x = cycle [x]

assignToGroups n aList = zip groups aList
  where
    groups = cycle [1 .. n]

subseq start end aList = take diff (drop start aList)
  where
    diff = end - start

inHalf val aList = val `elem` half
  where
    mid = (length aList) `div` 2
    half = take mid aList

myTake1 n list =
  if n > 0
    then head list : (myTake1 next (tail list))
    else []
  where
    next = n - 1

myGCD1 a b = 
  if remainder == 0
    then b
    else myGCD1 b remainder 
  where remainder = a `mod` b

sayAmount1 n = case n of
  1 -> "one"
  2 -> "two"
  _ -> "a bunch"

sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount _ = "a bunch"

myHead (x:xs) = x
myHead [] = error "Empty list in myHead"

myTail [] = []
myTail (x:xs) = xs

myTake 0 _ = []
myTake n [] = []
myTake n (x:xs) = x : myTake (n-1) xs

myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)