module EvalRPNTrans where

import Control.Monad.State
import Control.Applicative
import Text.Read (readMaybe)

type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
    xs <- get
    when (null xs) $ lift Nothing
    put (tail xs)
    pure (head xs)

