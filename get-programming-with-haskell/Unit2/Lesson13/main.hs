-- GHCi> :t typeToCheck

-- GHCi> :info TypeClass

-- GHCi> describe (5 :: Int)

class Describable a where
    describe :: a -> String

minInt = minBound :: Int

maxInt = maxBound :: Int

data IceCream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- Q13.3
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
                then minBound
                else succ n