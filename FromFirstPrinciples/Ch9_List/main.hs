
-- Using ranges to construct lists
--
enumFromTo' :: (Enum a) => a -> a -> [a]
enumFromTo' from to = go from to []
    where 
        go :: (Enum a) => a -> a -> [a] -> [a]
        go start end array
            | fromEnum(start) > fromEnum(end) = []
            | fromEnum(start) == fromEnum(end) = start:array
            | fromEnum(start) < fromEnum(end) = go start (pred end) (end : array)

etfBool :: Bool -> Bool -> [Bool]
etfBool = enumFromTo'

etfOrd :: Ordering -> Ordering -> [Ordering]
etfOrd = enumFromTo'

etfInt :: Int -> Int -> [Int]
etfInt = enumFromTo'

etfChar :: Char -> Char -> [Char]
etfChar = enumFromTo'

-- Extracting portions of lists
--
trimLeft :: String -> String
trimLeft = dropWhile (== ' ')

flipArray :: [a] -> [a]
flipArray arr = go arr []
    where 
        go :: [a] -> [a] -> [a]
        go [] output = output
        go (x:xs) output = go xs (x:output)

trimRight :: String -> String
trimRight = flipArray . trimLeft . flipArray

trim :: String -> String
trim = trimRight . trimLeft

myTool :: (a -> Bool) -> [a] -> [[a]]
myTool _ [] = []
myTool fn (x:xs)
    | fn x == False = myTool fn xs
    | otherwise = (takeWhile fn (x:xs)) : (myTool fn (dropWhile fn (x:xs)))

myWord :: String -> [String]
myWord = myTool (\x -> x /= ' ')

myWord' :: String -> [String]
myWord' str = go str []
    where
        go :: String -> [String] -> [String]
        go [] output = flipArray output
        go (' ':xs) output = go xs output
        go input output = go (dropWhile (/= ' ') input) ((takeWhile (/= ' ') input) : output)

myWord'' :: String -> [String]
myWord'' [] = []
myWord'' (' ':xs) = myWord'' xs
myWord'' xs = (takeWhile (/=' ') xs) : (myWord'' (dropWhile (/= ' ') xs))

myLines :: String -> [String]
myLines = myTool (\x -> x /= '\n')


myLines' :: String -> [String]
myLines' str = go str []
    where
        go :: String -> [String] -> [String]
        go [] output = flipArray output
        go ('\n':xs) output = go xs output
        go input output = go (dropWhile (/= '\n') input) ((takeWhile (/= '\n') input) : output)

myLines'' :: String -> [String]
myLines'' [] = []
myLines'' ('\n':xs) = myLines'' xs
myLines'' xs = (takeWhile (/= '\n') xs) : (myLines'' (dropWhile (/= '\n') xs))

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful\
    \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual = [
    "Tyger Tyger, burning bright" ,
    "In the forests of the night" ,
    "What immortal hand or eye" ,
    "Could frame thy fearful symmetry?"
    ]


main :: IO ()
main = 
    print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

-- List comprehensions
--
mySqr = [x^2 | x <- [1..10]]
-- 1, 4, 9, 16, 25, 36, 49, 64, 81, 100

ex1 = [x | x <- mySqr, rem x 2 == 0]
-- 4, 16, 36, 64, 100

ex2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- (1, 64), (1, 81), (1, 100),
-- (4, 64) ..
-- (9, 64) ..
-- (16, 64) ..
-- (25, 64) ..
-- (36, 64) ..

ex3 = take  4 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- (1, 64), (1, 81), (1, 100), (4, 64)
--

myCube = [y^3 | y <- [1..10]]

ex4 = [(x, y) | x <- mySqr, y <- myCube]

ex5 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- Zipping
--

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith (,)

zip'' [] _ = []
zip'' _ [] = []
zip'' (x:xs) (y:ys) = (x, y) : (zip' xs ys)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' fn (x:xs) (y:ys) = (fn x y) : (zipWith' fn xs ys)
