module Main where

-- 1.
e1_1 :: [Char]
e1_1 = ['a', 'b', 'c']

e1_2 :: (Char, Char, Char)
e1_2 = ('a', 'b', 'c')

e1_3 :: [(Bool , Char)]
e1_3 = [(False , '0'), (True, '1')]

e1_4 :: ([Bool], [Char])
e1_4 = ([False, True], ['0', '1'])

e1_5 :: [[a]-> [a]]
e1_5 = [tail, init, reverse]

-- 2. 
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1], [1,2], [1..3]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy v = (v, v)

apply :: (a -> b) -> a -> b
apply f = f 

-- 3.

second :: [a] -> a
second = head . tail

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double = (*2)

palindrom :: Eq a => [a] -> Bool
palindrom xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f = f . f

main :: IO ()
main = undefined