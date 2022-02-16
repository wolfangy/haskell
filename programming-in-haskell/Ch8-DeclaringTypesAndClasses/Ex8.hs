{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ex8 where

import Lib
import Prelude hiding (Ordering)

-- 1.

mult :: Nat -> Nat -> Nat
mult x Zero = Zero
mult x (Succ Zero) = x
mult x (Succ y) = add x $ mult x y

-- 2.

-- 3.
data Tree a where
  Leaf :: a -> Tree a
  Node :: Tree a -> Tree a -> Tree a
  deriving (Show)

listToTree :: [a] -> Tree a
listToTree [x] = Leaf x
listToTree (x : y : xs)
  | null xs = Node (Leaf x) (Leaf y)
  | otherwise = Node (Node (Leaf x) (Leaf y)) (listToTree xs)

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = (<= 1) . abs $ children l - children r
  where
    children (Leaf _) = 0
    children (Node l r) = children l + 1 + children r

-- 4.

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance $ take s xs) (balance $ drop s xs)
  where
    s = length xs `div` 2

-- 5.

data Expr where
  Val :: Int -> Expr
  Add :: Expr -> Expr -> Expr
  deriving (Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add l r) = g (folde f g l) (folde f g r)

-- 6.

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- 7.
{-
instance Eq a => Eq (Maybe a) where
    (==) Nothing Nothing = True
    (==) (Just _) Nothing  = False
    (==) Nothing (Just _) = False
    (==) (Just x) (Just y) = x == y
-}