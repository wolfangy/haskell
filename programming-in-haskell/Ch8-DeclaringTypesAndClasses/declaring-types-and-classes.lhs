> {-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, RankNTypes, ScopedTypeVariables #-}

8.1  Type declarations

> type Pos = (Int, Int)

> type Tans = Pos -> Pos

The namne of new type must begin with a captial letter.
Type declarations can be nested.

!! Type declaration cannot be recursive.

Type declarations can also be parameterised by other types.

> type Pair a = (a, a)

Type delcarations with more than one parameter are possible too.

> type Assoc k v = [(k, v)]

> find :: Eq a => a -> Assoc a b -> Maybe b
> find _ [] = Nothing
> find k ((key, val):xs) = if k == key
>                             then Just val
>                             else find k xs

8.2 Data declarations

A completely new type, as opposed to a synonym for an existing typ, 
can be declared by specifying its values using the data mechanism of Haskell.

> data Boolean = False | True

the symbol | is read as or, and the new values of the type are clalled constructors.

Values of the new types in Haskell can be used in precisely the same way as those of built-in
types.

data Move = North | South | East | West deriving Show

with GADT syntax:

> data Move where
>   North :: Move
>   South :: Move
>   East :: Move
>   West :: Move
>   deriving Show

> move :: Move -> Pos -> Pos
> move North (x, y) = (x, y+1)
> move South (x, y) = (x, y-1)
> move East (x, y) = (x + 1, y)
> move West (x, y) = (x - 1, y)

The constructors in a data declaration can also have arguments.

data Shape = Circle Float | Rect Float Float

> data Shape where
>   Circle  :: Float -> Shape
>   Rect    :: Float -> Float -> Shape
>   deriving Show

> square :: Float -> Shape
> square n = Rect n n

> area :: Shape -> Float
> area (Circle r) = pi * r ^ 2
> area (Rect l h) = l * h

Data delarations themselves can also be parameterised.

> data Option a = None | Some a

> data OptionGDT a where
>   NoneGDT :: OptionGDT a 
>   SomeGDT :: a -> OptionGDT a

> safeDiv :: Int -> Int -> OptionGDT Int
> safeDiv _ 0 = NoneGDT
> safeDiv m n = SomeGDT (m `div` n)


8.3 Newtype declarations

If a new type has a single constructor with a single argument, then
it can also be declared using the `newtype` mechanism.

> newtype Nat = N Int

> newtype Nat' where
>   N' :: Int -> Nat'
>   deriving Show
 
* using newtype meands that Nat & Int are different types rather than synonyms
    Haskell ensures that they cannot accidentally be mixed up in our programs.

* using newtype rather than data brings an efficiency benefit, because newtype 
    constructors such as N do not incur any cost when programs are evaluated.

8.4 Recursinve types

New types declared using data and newtype mechanisms can also be recursive.

> data RecursiveNat where
>   Zero :: RecursiveNat
>   Succ :: RecursiveNat -> RecursiveNat
>   deriving Show

> zero = Zero
> one  = Succ Zero
> two = Succ . Succ $ Zero

> three = Succ . Succ . Succ $ Zero

> nat2int :: RecursiveNat -> Int
> nat2int Zero = 0
> nat2int (Succ n) = 1 + nat2int n

> int2nat :: Int -> RecursiveNat
> int2nat 0 = Zero
> int2nat n = Succ $ int2nat (n-1)

> add' :: RecursiveNat -> RecursiveNat -> RecursiveNat
> add' x y = int2nat $ nat2int x + nat2int y

> add :: RecursiveNat -> RecursiveNat -> RecursiveNat
> add Zero n  = n
> add (Succ m) n = Succ $ add m n


> data Lst a where
>   Nil :: Lst a
>   Cons :: a -> Lst a -> Lst a
>   deriving Show

> len ::  Lst a -> Int
> len Nil  = 0
> len (Cons _ xs)  = 1 + len xs

> data Tree a where
>   Leaf :: a -> Tree a
>   Node :: Tree a -> a -> Tree a -> Tree a
>   deriving Show

> tree1 :: Tree Int
> tree1 = Node
>            (Node (Leaf 1) 3 (Leaf 4))
>            5
>            (Node (Leaf 6) 7 (Leaf 9))

      5
    /   \  
   3     7
  / \   / \
 1   4 6   9

> occurs :: Eq a => a -> Tree a -> Bool
> occurs v (Leaf l)     =  v == l
> occurs v (Node l n r) =  n == v || occurs v l || occurs v r

> flatten :: Tree a -> [a]
> flatten (Leaf l) = [l] 
> flatten (Node l n r) = flatten l ++ [n] ++ flatten r
