{-# LANGUAGE RankNTypes #-}
module NaturalTransformation where

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

type BadNat f g a = f a -> g a
maybeToList' :: BadNat Maybe [] a
maybeToList' Nothing = []
maybeToList' (Just a) = [a]


degenerateMtl :: Num a => BadNat Maybe [] a
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a+1]

