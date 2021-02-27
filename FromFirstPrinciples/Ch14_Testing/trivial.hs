module Main where 

-- To use QuickCheck in a command such as `stack ghci`
-- by adding the `--package` option
-- e.g.
-- stack ghci --package QuickCheck -- trivial.hs
--
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

main :: IO ()
main = do
    sample trivialGen
