module Main where

import Debug.Trace (trace)


-- Thunk life
--
thunk1 = [1, 2] :: [Integer]
-- :sprint thunk1

thunk2 = [1, 2]
-- :sprint thunk2

thunk3 = [1, 2, id 3] :: [Integer]
-- :sprint thunk3

thunk4 = thunk3 ++ undefined
-- :sprint thunk4


-- Sharing
--

inc = (+1)

twice = inc . inc

howManyTwice = inc (trace "eval'd" (1 + 1))
                + twice
                   (trace "eval'd" (1 + 1))

howManyTwice' = let onePlusOne = trace "eval'd" (1 + 1)
                in inc onePlusOne + twice onePlusOne

-- Name make GHC share something

name1 = trace "x" (1 :: Int)
name2 = trace "y" (1 :: Int)

sumVal = name1 + name2
sumVal' = (id name1) + (id name1)

-- Inline expression prevents sharing
f :: a -> Int
f _ = trace "f" 1

fa = f 'a'
fa' = f 'a'

a :: Int
a = trace "a" 2 + 2

b = (a + a)

c :: Int
c = (trace "a" 2 + 2) + (trace "a" 2 + 2)

-- Being a function with explicit, named arguments also prevents sharing
--
-- sharing
ff :: a -> Int
ff = trace "f" const 1

-- no sharing
ff' :: a -> Int
ff' x = trace "f" const 1 x

-- Typeclass constrains also prevent sharing
--

blah = Just 1
blah' = fmap ((+1) :: Int -> Int) blah

bl :: Num a => Maybe a
bl = Just (trace "bl" 1)
bl' = fmap (+1) bl
bl'' = fmap ((+1) :: Int -> Int) bl

-- the typeclass constraints get simplified to the underlying GHC Core language,
-- they're really function arguments
-- The only way to apply that function argument is to reach an expression that
-- provides a concrete type satisfying the constraints
--
-- Polymorphic expression can't be shared
--
poly :: Num a => a
poly = 1

conc = poly :: Int


blFn :: Int -> Int
blFn x = x + 1

-- Using a lambda that mentions the argument in some fashion disable sharing

fx x = (x ()) + (x ())

-- Evaluate twice
execFx = fx (\_ -> trace "hi" 2)

-- Evaluate once
execFx' = fx (const (trace "hi" 2))

-- functions aren't shared when there are named arguments 
-- but are when the arguments are elided

main :: IO ()
main = print (name1 + name2)
