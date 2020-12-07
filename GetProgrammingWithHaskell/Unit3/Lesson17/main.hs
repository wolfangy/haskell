import Data.Semigroup

myLast = head . reverse

-- myMin = head . sort

-- myMax = myLast . sort

myAll:: (a -> Bool) -> [a] -> Bool
myAll predicate = (foldr (&&) True) . (map predicate)


myAny :: (a -> Bool) -> [a] -> Bool
myAny predicate = (foldr (||) False) . (map predicate)


data Color = 
    Transparent |
    Red |
    Blue |
    Green |
    Purple |
    Orange |
    Yellow |
    Brown deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b 
        | a == b = a
        | a == Transparent = b
        | b == Transparent = a
        | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
        | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
        | all (`elem` [Red, Yellow, Orange]) [a,b] = Orange
        | otherwise = Brown

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
    where 
        totalProbs = sum probs
        normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where
        nToAdd = length l2
        repeatedL1 = map ((take nToAdd) . repeat) l1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle l2

combineEvents e1 e2 = cartCombine combiner e1 e2
    where
        combiner = (\ x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
    (<>) ptable (PTable [] []) = ptable
    (<>) (PTable [] []) ptable = ptable
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where 
            newEvents = combineEvents e1 e2
            newProbs = combineProbs p1 p2

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]


-- Q17.1
instance Monoid Color where
    mempty = Transparent
    mappend = (<>)


-- Q17.2
data EventsV2 = EventsV2 [String]
data ProbsV2 = ProbsV2 [Double]

data PTableV2 = PTableV2 EventsV2 ProbsV2

createPTableV2 :: [String] -> [Double] -> PTableV2
createPTableV2 events probs = PTableV2 (EventsV2 events) (ProbsV2 normalizedProbs)
    where
        totalProbs = sum probs
        normalizedProbs = map (/ totalProbs) probs


instance Show PTableV2 where
    show (PTableV2 (EventsV2 events) (ProbsV2 probs)) = mconcat pairs
        where pairs = zipWith showPair events probs

combineEventsV2 :: EventsV2 -> EventsV2 -> EventsV2
combineEventsV2 (EventsV2 e1) (EventsV2 e2) = EventsV2 (cartCombine combiner e1 e2)
    where
        combiner = (\ x y -> mconcat [x, " - ", y])

combineProbsV2 :: ProbsV2 -> ProbsV2 -> ProbsV2
combineProbsV2 (ProbsV2 p1) (ProbsV2 p2) = ProbsV2 (cartCombine (*) p1 p2)
