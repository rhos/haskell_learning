x :: Int
x = 2

y :: Integer
y = 2

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
myAverage list = fromIntegral(sum list) / fromIntegral (length list)
