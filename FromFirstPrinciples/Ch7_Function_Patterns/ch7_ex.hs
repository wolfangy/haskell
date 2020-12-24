module Ch7_Ex where

-- 1

tenDigit :: Integral a => a -> a
tenDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod`10

tenDigit' :: Integral a => a -> a
tenDigit' x = d
    where (xLast, _) = x `divMod` 10
          (_, d)     = xLast `divMod` 10

hunsDigit :: Integral a => a -> a
hunsDigit x = h
    where xLast = x `div` 100
          h     = xLast `mod` 10

-- 2.

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
    True -> y
    False -> x

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
    | b == True = y
    | b == False = x

-- 3.
--
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


-- 4.

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
    print (roundTrip 4)
    print (id 4)

-- 5.
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

-- 6.
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show
