import Control.Monad.RWS
import System.Random

type Dice = Int

type DiceGame = RWS (Int, Int) [Dice] StdGen

dice :: DiceGame Dice
dice = do
    bs <- ask
    g <- get
    let (r, g') = uniformR bs g
    put g'
    tell [r]
    return r

dice' :: DiceGame Dice
dice' = do
    bs <- ask
    r <- state (uniformR bs)
    tell [r]
    pure r

dice'' :: DiceGame Dice
dice'' = ask >>= state . uniformR >>= \r -> tell [r] >> pure r

doubleDice :: DiceGame (Dice, Dice)
doubleDice = (,) <$> dice <*> dice

dices :: Int -> DiceGame [Dice]
dices n = replicateM n dice

dices' :: Int -> DiceGame [Dice]
dices' 0 = (:[]) <$> dice
dices' n = (:) <$> dice <*> (dices' $ n - 1)

-- replicateM :: Applicative m            => Int -> m a -> m [a]
-- forM       :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)

showDices :: DiceGame [Dice] -> IO ()
showDices ds = do
    g <- newStdGen
    let v = evalRWS ds (1, 6) g
    print v

main :: IO ()
main = undefined