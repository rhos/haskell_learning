calcChange owed given = if change > 0
                        then change
                        else 0
                        where change = given - owed

isEven n = if even n
    then n - 2
    else 3*n+1