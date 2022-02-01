module Lib
    ( 
        foldA
    ) where

import Data.Array
import Data.Ix

foldA :: Ix k => (b -> a -> b) -> b -> Array k a -> b
foldA f s a = go s (indices a)
    where
        go s (x:xs) =
            let s' = f s (a ! x)
            in s' `seq` go s' xs
        go s _ = s

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a