- Double  Double-precision floating point. A common choice for floating-point data.
- Float Single-precision floating point. Often used when interfacing with C.
- Int   Fixed-precision signed integer; minimum range  [-2^29..2^29-1]. Commonly used
- Int8  8-bit signed integer.
- Int16 16-bit signed integer.
- Int32 32-bit signed integer.
- Int64 64-bit signed integer.
- Integer   Arbitrary-precision signed integer; range limited only by machine resources. Commonly used.
- Rational  Arbitrary-precision rational numbers. Stored as a ratio of two Integers
- Word  Fixed-precision unsigned integer; storage size same as Int.
- Word8 8-bit unsigned integer.
- Word16    16-bit unsigned integer.Word3232-bit unsigned integer.
- Word64    64-bit unsigned integer.

---

**Num**
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}

**Enum**
type Enum :: * -> Constraint
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}

**Read**
type Read :: * -> Constraint
class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  GHC.Read.readPrec :: Text.ParserCombinators.ReadPrec.ReadPrec a
  GHC.Read.readListPrec :: Text.ParserCombinators.ReadPrec.ReadPrec
                             [a]
  {-# MINIMAL readsPrec | readPrec #-}

type ReadS :: * -> *
type ReadS a = String -> [(a, String)]
        -- Defined in `Text.ParserCombinators.ReadP'

---

**Fractional**
type Fractional :: * -> Constraint
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  {-# MINIMAL fromRational, (recip | (/)) #-}

**Floating**
type Floating :: * -> Constraint
class Fractional a => Floating a where
  pi :: a
  exp :: a -> a
  log :: a -> a
  sqrt :: a -> a
  (**) :: a -> a -> a
  logBase :: a -> a -> a
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  asin :: a -> a
  acos :: a -> a
  atan :: a -> a
  sinh :: a -> a
  cosh :: a -> a
  tanh :: a -> a
  asinh :: a -> a
  acosh :: a -> a
  atanh :: a -> a
  GHC.Float.log1p :: a -> a
  GHC.Float.expm1 :: a -> a
  GHC.Float.log1pexp :: a -> a
  GHC.Float.log1mexp :: a -> a
  {-# MINIMAL pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh,
              asinh, acosh, atanh #-}

** Num => Fractional => Floating **
-  `instance Floating Float`
-  `instance Floating Double`

---

**Real**
type Real :: * -> Constraint
class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
  {-# MINIMAL toRational #-}

**Integral**
type Integral :: * -> Constraint
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  {-# MINIMAL quotRem, toInteger #-}

**`toInteger`** convert any Integral to Integer