{-# LANGUAGE StandaloneDeriving #-}
import System.Random
import System.Random.Stateful (uniformRM, uniformM)

import Radar
import Control.Monad (replicateM)

deriving instance Ord Turn

instance UniformRange Turn where
    uniformRM (lo, hi) rng = do
        res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
        pure $ toEnum res

instance Uniform Turn where
    uniformM rng = uniformRM (minBound, maxBound) rng

instance UniformRange Direction where
    uniformRM (lo, hi) rng = do
        res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
        pure $ toEnum res

instance Uniform Direction where
    uniformM rng = uniformRM (minBound, maxBound) rng

uniformIO :: Uniform a => IO a
uniformIO = getStdRandom uniform

uniformsIO :: Uniform a => Int -> IO [a]
uniformsIO n = replicateM n uniformIO

randomTurns :: Int -> IO [Turn]
randomTurns = uniformsIO

randomDirections :: Int -> IO [Direction]
randomDirections = uniformsIO

writeRandomFile :: (Random a, Show a) => Int -> (Int -> IO [a]) -> FilePath -> IO ()
writeRandomFile n gen fname = do
    xs <- gen n
    writeFile fname $ unlines $ map show xs