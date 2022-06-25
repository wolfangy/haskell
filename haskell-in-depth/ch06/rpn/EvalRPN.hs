module EvalRPN where

import Control.Monad.State

type Stack = [Integer]
type EvalM = State Stack

push :: Integer -> EvalM ()
push x = modify (x:)

pushTo :: EvalM () -> Integer -> EvalM ()
pushTo st v = withState append st
    where append stack = v : stack

pop :: EvalM Integer
pop = do
    xs <- get
    put $ tail xs
    return $ head xs

evalRPN :: String -> Integer
evalRPN expr = evalState evalRPN' []
    where
        evalRPN' = traverse step (words expr) >> pop

        step "+" = processTops (+)
        step "-" = processTops (-)
        step "*" = processTops (*)
        step t = push . read $ t

        processTops op = flip op <$> pop <*> pop >>= push
