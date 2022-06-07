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


:exclamation: GHCi provides two commands to get information about type classes: `:info` and `:doc`


Deriving Instances

* We can derive instances automatically.

* We can implement those instance manually.
