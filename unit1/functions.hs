ifEven f x =
  if even x
    then f x
    else x

compareByLast tuple1 tuple2 =
  if snd1 < snd2
    then GT
    else if snd1 > snd2
           then LT
           else EQ
  where
    snd1 = snd tuple1
    snd2 = snd tuple2

getLocationFunction location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

sfOffice name =
  if lastName < "L"
    then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

dcOffice name = nameText ++ ", Esq."
  where
    nameText = snd name

addressLetter name location = locationFunction name
  where
    locationFunction = getLocationFunction location
