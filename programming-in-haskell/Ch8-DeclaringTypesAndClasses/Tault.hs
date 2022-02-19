{-# LANGUAGE GADTs #-}

module Tault where

import Data.Maybe
import qualified Lib as L

data Prop where
    Const   :: Bool -> Prop
    Var     :: Char -> Prop
    Not     :: Prop -> Prop
    And     :: Prop -> Prop -> Prop
    Or      :: Prop -> Prop -> Prop
    Imply   :: Prop -> Prop -> Prop
    Equiv   :: Prop -> Prop -> Prop
    deriving (Eq, Show)

type Subst = L.Assoc Char Bool

find :: Char -> Subst -> Bool
find c s = fromMaybe False (L.find c s)

-- p1 : A ^ ~A
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- p2 : (A ^ B) => A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

--p3 : A => (A ^ B)
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- p4 : (A ^ (A => B)) => B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

eval :: Subst -> Prop -> Bool
eval _ (Const b)    = b
eval s (Var x)      = find x s
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q
eval s (Or p q)     = eval s p || eval s q
eval s (Imply p q)  = eval s p <= eval s q
eval s (Equiv p q)  = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)      = []
vars (Var x)        = [x]
vars (Not p)        = vars p
vars (And p q)      = vars p ++ vars q
vars (Or p q)       = vars p ++ vars q
vars (Imply p q)    = vars p ++ vars q
vars (Equiv p q)    = vars p ++ vars q


bools' :: Int -> [[Bool]]
bools' n = map (reverse . map conv . make n . L.int2bin) range
    where
        range       = [0..(2^n)-1]
        make n bs   = take n (bs ++ repeat 0)
        conv 0      = False
        conv 1      = True

{- 
bool 3 contains two copies of bool 2:

    False   |   False   False
    False   |   False   True
    False   |   True    False
    False   |   True    True
    -----------------------------
    True    |   False   False
    True    |   False   True
    True    |   True    False
    True    |   True    True
-}

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True:) bss
    where bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = L.rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = all (`eval` p) (substs p)

isTaut' p = and [ eval s p |s <- substs p]