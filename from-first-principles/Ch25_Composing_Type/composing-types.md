# Chapter 25 Composing Types

## 25.1 Composing types

## 25.2 Composing functions as types

## 25.3 Two Functors: Lifting


## 25.4 Two Applicatives

## 25.5 Two Monads

## 25.6 Exercises

## 25.7 Monad Transfers

:exclamation: __A monad transformer__ is __a type constructor__ that takes monad 
as an argument and returns a monad as a result:

In order to make the _join_ happen:

* we need to reduce the polymorphism and get concrete information about one of the monads that we're working with.

* The other monad remains polymorphic as a variable type argument to our type constructor.


### Monadic stacking

We want a `>>=` that can address more than one `Monad` at once, like: `IO (Reader String [a])`

## 25.8 IdentityT

```haskell
newtype Identity a =
    Identity {runIdentity :: a}
    deriving (Eq, Show)

newtype IdentityT f a =
    IdentityT {runIdentityT :: f a}
    deriving (Eq, Show)
```

## 25.9 Finding a pattern

Transformers is been used to address:

When we want a `>>=` operation over `f` and `g` of different types, but both have `Monad` instances.

* To compose two polymorphic types `f` and `g`, will be end up with: `f (g (f b))`;

* `Monad` bind cannot `join` these types, to get `f (f b)`

* Unless we have some way to `folding` the `g` in the middle

:sparkles: __Make `g` concrete__: with concrete type information for the inner bit of structure, we can fold out the g and get on with it.

:sparkles: __`f` can remain polymorphic__ : so we only need to write a transformer once for each type.

The basic pattern that many monad transformers are enabling us to cope with entails the following type transitions:

:point_right: `T` is outer structure and some concrete type the transformer is for
:point_right: `m` is the polymorphic

```haskell
    m (T m b)
->  m (m b)
->  m b
->  T m b
```