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
