module Radar where

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d
    
    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d

data Direction where
    North :: Direction
    East :: Direction
    South :: Direction
    West ::Direction
    deriving (Eq, Enum, Bounded, CyclicEnum, Show)

data Turn where
    TNone :: Turn
    TLeft :: Turn
    TRight :: Turn
    TAround :: Turn
    deriving (Eq, Enum, Bounded, CyclicEnum, Show)

instance Semigroup Turn  where
    TNone <> t = t
    TLeft <> TLeft = TAround
    TLeft <> TRight = TNone
    TLeft <> TAround = TRight
    TRight <> TRight = TAround
    TAround <> TAround = TNone
    t1 <> t2 = t2 <> t1

instance Monoid Turn where
    mempty = TNone

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred


every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every


rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate) 

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

orientMany ::[Direction] -> [Turn]
orientMany ds@(_:_:_) = zipWith orient ds (tail ds)
orientMany _ = []

