# Monad Transformers

## 26.1 Monad transformers

* More monad transformer types and instance

* Ordering and wrapping of monad transformer stacks

* Lift

## 26.2 MaybeT

```haskell
newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }
```

## 26.3 EitherT

```haskell
newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a)}
```

## 26.4 ReaderT

```haskell
newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a}
```


## 26.5 StateT

```haskell
newtype StateT s m a =
    StateT { runStateT :: s -> m (a, s) }
```

### WriterT

```haskell
newtype WriterT w m a = 
    WriterT { runWriterT :: m (a, w) }
```

:firecracker: `Writer` let us deal with values we can emit and combine (but not read), and `State` lets us both read and write in any manner we desire - including __monoidally__ like `Writer`.

### RWST

```haskell
newtype RWST r w s m = 
    RWST { runRWST :: r -> s -> m (m, s, w) }
```

### Parser

a simple parser type:

```haskell
type Parser a = String -> Maybe (a, String)
```

Now it could be defined as:

```haskell
type Parser = StateT String Maybe
```

## 26.6 Types do not use

__`Writer`__ is either too lazy or too strict for the problem:

1. `Writer` can accumulate unevaluated thunks, causing memory leaks.

2. `Writer` is inappropriate for logging long-running or on-going programs:
you cannot retrieve any of the logged values until the computation is complete.

__`ListT`__ is __NOT__ very fast, Streaming library like `pipes` and `conduit` do it better.


## 26.7 An ordinary type from a transformer

```haskell
type Identity a = IdentityT Identity a
type Maybe    a = MaybeT    Identity a
type Either e a = EitherT e Identity a
type Reader r a = ReaderT e Identity a
type State  s a = StateT  s Identity a
```

## 26.8 Lexically inner is structurally outer

```haskell
newtype ExceptT e m a =
    ExceptT { runExceptT :: m (Either e a) }

newtype MaybeT m a =
    MaybeT { runMaybe :: m (Maybe a) }

newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a}

embeded :: MaybeT (ExceptT String (ReaderT () IO)) Int
```

