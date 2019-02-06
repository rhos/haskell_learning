sumSOSS x y =   if sumSquare > squareSum
                then sumSquare
                else squareSum
    where   sumSquare = x^2 + y^2
            squareSum = (x+y)^2

lSOSS x y = (\sumSquare squareSum ->
                if sumSquare > squareSum
                then sumSquare
                else squareSum) (x^2 + y^2) ((x+y)^2)

overwrite x = 
    (\x -> 
        (\x -> 
            (\x -> x) 4 
        )3
    )2

-- counter x = 
    -- let x = x + 1
    -- in
        -- let x = x + 1
        -- in
        -- x

counter x =
    (\x -> 
        (\x -> 
            x + 1
        ) x + 1
    ) x