# Chapter 2 Type classes

Chapter converts:

* Exploiting type classes as tools for writing code that works for values of different typs

* Considering type classes as a concept applicable to many types

* Using basic type classes defined in the standard library

* Abstracting computations via type classes

----

:+1: __`Type classes`__ are usually considered most prominent feature:

* With respect to some type variable

* Contains a collection of methods, given by signatures

* Can define as many instances or implementation as we needed

:+1: Writing functions with respect to a type class, as opposed to using concrete types, allows to be more general. This makes type classes especially useful for _libraries_.

## 2.1 Manipulating a radar antenna with type classes

:+1: It is useful to think about type classes as abstract concepts

:+1: An experienced Haskeller often looks for a type class first and then start coding.

### 2.1.1 The problem at hand

**Example ch02/radar**

A radar antenna state and behavior:

```haskell
data Direction = North | East | South | West
data Turn = TNone | TLeft | TRight | TAround

rotate :: Turn -> Direction -> Direction
orient :: Direction -> Direction -> Turn
```

### 2.1.2 Rotating a radar antenna with Eq, Enum and Bounded

:crystal_ball: The concept of `enumerating` data constructors expressed with the __`Enum`__ type class.

`Enum` has eight methods:

```haskell
class Enum a where
    succ :: a -> a
    pred :: a -> a

    toEnum   :: Int -> a
    fromEnum :: a -> Int

    enumFrom        :: a -> [a]
    enumFromThen    :: a -> a -> [a]
    enumFromTo      :: a -> a -> [a]
    enumFromThenTo  :: a -> a -> a -> [a]
```

:point_right: If it makes sense to enumerate elements of a type one by one, then we should implement `Enum`.


:crystal_ball: __`Bounded`__ is used to specify `minimum` and `maximum` bounds among all the data constructors.

`Bounded` class defines two methods:

```haskell
class Bounded a where
    minBound :: a
    maxBound :: a
```

:point_right: If a data type supports the idea of `minimum` and `maximum`, then we define the `Bounded` instance.


:crystal_ball: __`Eq`__ to tell whether two values are equal.

`Eq` type class with two methods: `(==)` and `(/=)`

```haskell
class Eq a where
    (==), (/=) :: a -> a -> Bool
```

:point_right: If the value of a type can be checked for equality, we define an instance of the `Eq` type class.

> :exclamation: GHCi provides two commands to get information about type classes: `:info` and `:doc`

#### Deriving Instances

* We can derive instances automatically.

* We can implement those instance manually.

```haskell
data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, Show)

data Turn = TNone | TLeft | TRight | TAround
    deriving (Eq, Enum, Bounded, Show)

> fromEnum West
-- 3

> toEnum 3 :: Direction
-- West
```

#### Building Abstractions Upon Abstractions

Class defines two methods with default implementations:

```haskell
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise     = pred d
    
    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise     = succ d
```

To allow use of any typeclass in `deriving` class, we need to use __`DeriveAnyClass`__:

```haskell
{-# LANGUAGE DeriveAnyClass #-}

data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, CyclicEnum, Show)
```

The above code is equivalent to:

```haskell
data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, Show)

instance CyclicEnum Direction
```

#### Implementing Radar Manipulation Functions

As the `Turn` is a instance of `Enum`, so the list of `Turn` can be shorten:

```haskell
[TNone, TLeft, TRight, TAround]
-- equivalent to:
[TNone..TAround]
```

As `Turn` data type also is an instance of `Bounded`, so it can be expressed as:

```haskell
[minBound..maxBound]
```

This behavior can be generalized to a generic function:

```haskell
every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound
```

> Important list processing functions:
>
> ```haskell
> foldl :: (b -> a -> b) -> b -> [a] -> b
>
> scanl :: (b -> a -> b) -> b -> [a] -> [b]
>
> zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
> ```

### 2.1.3 Combining turns with Semigroup and Monoid

#### Semigroup

Defined in __Data.Semigroup__

```haskell
class Semigroup a where
    (<>) :: a -> a -> a
```

#### Monoid

```haskell
class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
```

`mempty` so-called `neutral` element, as it is expected to satisfy the _`monoid laws`_:

```haskell
mempty <> a = a
a <> mepmty = a
```

`mconcat` function return `mempty` if the given list is empty and applies an operation over all the elements from left to right otherwise.

`sconcat` function is similar but there is __NO neutral__ element in `Semigroup` so it cannot return something meaningful in the case of an empty list.

```haskell
sconcat :: Semigroup a => NonEmpty a -> a

> import Data.List.NonEmpty

data NonEmpty a = a :| [a]

> xyz = sconcat ("x" :| ["y", "z"])
-- "xyz"
```

After `Turn` implement `Semigroup` and `Monoid` type class, the following code are equivalent, and the later one only need to `rotate` once:

```haskell
rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate) 

rotateMany :: Direction -> [Turn] -> Direction
rotateMany dir ts = rotate (mconcat ts)
```

#### :exclamation: __OverloadedStrings__ GHC Extension

```haskell
> :t "Hello"
-- "Hello" :: [Char]

> :set -XOverloadedStrings
> :t "Hello"
-- "Hello" :: Data.String.IsString p => p
```

The `IsString` type class defines only one method: `fromString`

```haskell
class Data.String.IsString a where
  Data.String.fromString :: String -> a
```

:mage: The only thing the extension `OverloadedStrings` is responsible for is replacing every __string literal__ in the source code with a call to the `fromString` method on that literal.

:point_right: To disable the `OverloadedStrings` in GHCi:

```haskell
> :set -XNoOverloadedStrings
```

### 2.1.4 Printing and reading data with _Show_ and _Read_

