takeV1 :: Integer -> [a] -> [a]
takeV1 0 _ = []
takeV1 _ [] = []
takeV1 count (x:xs) = x : (takeV1 count-1 xs)


myGcd a b =
    if remainder == 0
        then b
        else myGcd b remainder
    where remainder = a `mod` b

sayAmount n = case n of
    1 -> "one"
    2 -> "two"
    n -> "a bunch"

isEmpty [] = True
isEmpty _ = False

myHead (x:xs) = x
myHead [] = error "No head for empty list"

myTail (_:xs) = xs
myTail [] = error "No tail for empty list"

-- Q7.2
myGcdV1 a b = 
    case remainder of
        0 -> b
        _ -> myGcdV1 b remainder
    where remainder = a `mod` b