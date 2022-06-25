module StateTest where

import Data.Foldable
import Control.Monad.State

addItem :: Integer -> State Integer ()
addItem n = do
    s <- get
    put (s + n)

addItem' :: Integer -> State Integer ()
addItem' n = modify' (+n)

sumList :: [Integer] -> State Integer ()
sumList xs = 
    traverse_ addItem xs
