ifEven f x =
  if even x
    then f x
    else x

genIfXEven x = (\f -> ifEven f x)

getRequestURL host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host =
  (\apiKey resource id -> getRequestURL host apiKey resource id)

genApiRequestBuilder hostBuilder api =
  (\resource id -> hostBuilder api resource id)

genResourceRequestBuilder apiBuilder resource = (\id -> apiBuilder resource id)

flipBinaryArgs f = (\x y -> f y x)

binaryPartialApplication f x = (\y -> f x y)
