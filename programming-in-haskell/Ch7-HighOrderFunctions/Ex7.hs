{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ex7 where
import Distribution.Simple.Command (OptDescr(BoolOpt))

-- 1
mapFilter = map . filter

-- 2

-- a
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr ((&&) . p) True

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs)
    | p x = True
    | otherwise = any' p xs


takeWhile' :: forall a. (a -> Bool) -> [a] -> [a]
takeWhile' p xs = takeWhileImpl p xs []
    where
        takeWhileImpl :: (a -> Bool) -> [a] -> [a] -> [a]
        takeWhileImpl _ [] result  = result
        takeWhileImpl p (x:xs) result
                    | p x  = takeWhileImpl p xs (result ++ [x])
                    | otherwise  = result

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p = foldr (\cur acc -> if p cur then cur : acc else []) []

dropWhile' :: forall a. (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
        | p x = dropWhile' p xs
        | otherwise = x:xs
-- 3
mapF :: Foldable t => (a -> b) -> t a -> [b]
mapF f = foldr ((:) . f) []

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldr (\a b -> if p a then a:b else b) []


-- 4

dec2int :: [Int] -> Integer
dec2int ds = sum [fromIntegral (w * b) | (w, b) <- zip weights ds]
    where weights = iterate (*10) 1

-- 5

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

-- 6 

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold init map next seed
    | init seed = []
    | otherwise = map seed : unfold init map next (next seed)

int2bin = reverse . unfold (== 0) (`mod` 2) (`div` 2)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

mapWithUnfold :: forall a b. (a -> b) -> [a] -> [b]
mapWithUnfold f = unfold null mapOne tail
    where
        mapOne :: [a] -> b
        mapOne = f . head

iterateWithUnfold :: (a -> a) -> a -> [a]
iterateWithUnfold f = unfold (const False) f f


-- 9.

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap l _ [x] = [l x]
altMap l r (x1:x2:xs) = [l x1, r x2] ++ altMap l r xs


altMap' l r xs = foldr (\(i, v) acc -> if even i then r v : acc else l v : acc) [] ys
    where ys = zip [1..] xs