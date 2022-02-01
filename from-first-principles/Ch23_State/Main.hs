module Main where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import System.Random

-- stack ghci --package transformers --package random -- Main.hs
--

-- mkStdGen :: Int -> StdGen
-- data StdGen =
--      System.Random.StdGen Int32 Int32
-- instance RandomGen StdGen

sg_1_1 = mkStdGen 0

-- next :: RandomGen g => g -> (Int, g)
nextSgPair = next sg_1_1
nextSgPair' = next sg_1_1
-- :t nextSg_1_1 :: (Int, StdGen)

newSg = snd nextSgPair

nextNextSgPair = next newSg

-- class RandomGen g where
--  next :: g -> (Int, g)
--  split :: g -> (g, g)
-- Defined in `System.Random`
-- instance RandomGen StdGen

randomIntPair = random newSg :: (Int, StdGen)
randomDoublePair = random newSg :: (Double, StdGen)

-- a number within a range

randomRPair = randomR (0,3) newSg
randomRIntPair = randomRPair :: (Int, StdGen)

-- class Random a where
--  randomR :: RandomGen g => (a, a) -> g -> (a, g)
--  random :: RandomGen g => g -> (a, g)
--  randoms :: RandomGen g => g -> [a]
--  randomIO :: IO a
-- MINIMAL: randomR, random
--

-- Isomorphism: there must be a way to go from the newtype to the thing it wraps 
-- and back again without losing information.
--

type Iso a b = (a -> b, b -> a)

sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)

-- newtype State s a = State { runState :: s -> (a, s) }
--
-- State is a function that takes an input state and returns an output value, a,
-- tupled with the new state value
--
-- The key is that the previous state value from each application is chained
-- to the next one, and this is not an uncommon pattern.
--
-- State is often used for things carrying working memory while traversing a 
-- data structure.
--

data Die =
    DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

initToDie :: Int -> Die
initToDie n =
    case n of 
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        x -> error $ "initToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    let s = mkStdGen 0
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, s3) = randomR (1, 6) s2
    (initToDie d1, initToDie d2, initToDie d3)

-- exp 2
--
-- state :: Monad m => (s -> (a, s)) -> StateT s m a

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (initToDie n, s)

-- the `state` function is a constructor that take a State-like function and 
-- embeds it in  the State monad transformer.
--
-- state :: Monad m => (s -> (a, s)) -> StateT s m a
--
rollDie' :: State StdGen Die
rollDie' = initToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

rollDieThreeTimesResult' = evalState rollDieThreeTimes' (mkStdGen 0)

repeatDie :: State StdGen [Die]
repeatDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where 
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= 20 = count
            | otherwise =
                let (die, nextGen) = randomR (1,6) gen
                in go (sum + die) (count + 1) nextGen

-- Ex 
--
-- 1.
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= limit = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen

-- 2.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit g = go 0 0 g []
    where
        go :: Int -> Int -> StdGen -> [Die] -> (Int, [Die])
        go sum count gen arr
            | sum >= limit = (count, arr)
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen (initToDie die:arr)

newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap f (Moi g) = Moi $ do
        a <- f . fst . g
        s <- snd . g
        return (a, s)--Moi (\s -> (f (fst(g s)), snd (g s)))

instance Applicative (Moi s) where
    pure a = Moi (\s -> (a, s))

    -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    -- f :: s -> (a -> b, s)
    -- g :: s -> (a, s)
    -- Moi s b :: Moi s -> (b , s)
    Moi f <*> Moi g = Moi (\s -> (,) ((fst $ f s) $ fst $ g s) s)

instance Monad (Moi s) where
    return = pure

    -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    -- Moi f => f :: s -> (a , s)
    -- g :: a -> Moi (s -> (b, s))
    (Moi f) >>= g = Moi $
                    \s -> let (a, s') = f s
                          in runMoi (g a) s'

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise       = show n


fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

-- runState :: State s a -> s -> (a , s)
-- Unwrap a state monad computation as a function
--
-- evalState ::  State s a -> s -> a
-- Evaluate a state computation with the given initial state and return the final value,
-- discarding the final state.
--
-- evalState m s = fst (runState m s)
--
-- execState :: State s a -> s -> s
-- Evaluate a state computation with the given initial state and return the final state,
-- discarding the final value.
--
-- execState m s = snd (runState m s)
--
-- mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
-- Map both the return value and final state of a computation using the given function
--
-- runState (mapState f m) = f . runState m
--
-- withState :: (s -> s) -> State s a -> State s a
-- withState f m executes actions m on a state modified by applying f.
--
-- withState f m = modify f >> m
--
--


addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

main :: IO()
main = mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]
