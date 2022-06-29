import Control.Monad.State
import Data.Foldable

addItem :: Integer -> State Integer ()
addItem n = do
    s <- get
    put (s + n)

addItemFromState :: Integer -> State Integer Integer
addItemFromState n = do
    prev <- get
    put $ prev + n
    return $ prev + n

addItem' :: Integer -> State Integer ()
addItem' n = modify' (+n)

sumList :: [Integer] -> State Integer ()
sumList xs = traverse_ addItem xs