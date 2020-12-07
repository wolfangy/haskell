myDrop :: Integer -> [a] -> [a]
myDrop 0 aList = aList
myDrop c (x:xs) = myDrop (c - 1) xs

myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = (+) 1 (myLength xs)

myCycle :: [a] -> [a]
myCycle aList = 
    let 
        cycleImpl = (\lst -> case lst of
            [] -> cycleImpl aList
            (x:xs) -> x : cycleImpl(xs))
    in
        cycleImpl aList

myCycleV2 :: [a] -> [a]
myCycleV2 (x:xs) = x : myCycleV2(xs ++ [x])

ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))


collatz 1 = 1
collatz n = 
    if even n
        then 1 + collatz (n `div` 2)
        else 1 + collatz (n * 3 + 1)

collatzList = map collatz [100..120]


-- Q8.1
myReverse [] = []
myReverse (x:xs) =
    myReverse(xs) ++ [x]

-- Q8.2
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fastFib n1 n2 1 = n1
fastFib n1 n2 2 = n2
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)
myFib = fastFib 1 1