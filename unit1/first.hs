main = do
    print "q1:"
    q1 <- getLine
    print "q2:"
    q2 <- getLine
    print "q3:"
    q3 <- getLine
    print(ft q1 q2 q3)

fq1 r = "q1: " ++ r ++ ",\n"
fq2 r = "q2: " ++ r ++ ",\n"
fq3 r = "q3: " ++ r ++ ",\n"
ft a b c = fq1 a ++ fq2 b ++ fq3 c