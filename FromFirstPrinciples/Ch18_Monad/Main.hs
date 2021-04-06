module Main where

import Data.Functor
import Control.Applicative
import Control.Monad

-- join :: Monad m => m (m a) -> m a
--
myBind :: Monad m => (a -> m b) -> m a -> m b
myBind f m = join $ fmap f m

-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftM :: Applicative m => (a1 -> r) -> m a1 -> m r

aP35 = liftA2 (,) (Just 3) (Just 5)
mP35 = liftM2 (,) (Just 3) (Just 5)

-- (*>) :: Applicative f => f a -> f b -> f b
-- (>>) :: Monad m => m a -> m b -> m b

sequencing :: IO()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

sequencing' :: IO()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

sequencing'' :: IO()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"

binding :: IO()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' =
    getLine >>= putStrLn

-- derive functor's fmap from monad
--
myFmap f ma = ma >>= return . f

printOne = putStrLn "1"
printTwo = putStrLn "2"

twoAction = (printOne, printTwo)

bindingAndSequencing :: IO ()
bindingAndSequencing = 
    putStrLn "name pls: " >>
    getLine >>= (\name -> putStrLn ("y helo thar: " ++ name))

twoBinds :: IO ()
twoBinds = 
    putStrLn "name pls:" >>
    getLine >>= 
    \name ->
    putStrLn "age pls:" >>
    getLine >>=
    \age ->
    putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

twiceWhenEven :: [Integer] -> [Integer ]
twiceWhenEven xs =
    xs >>=
    \x -> if even x then [x*x, x*x] else [x*x]

data Cow = Cow {
      name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n 
    | n >= 0 = Just n
    | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in
    if n == "Bess" && w > 499
    then Nothing
    else Just c


mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    noEmpty name' >>= 
    \n ->
    noNegative age' >>=
    \a ->
    noNegative weight' >>=
    \w ->
    weightCheck $ Cow n a w

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    n <- noEmpty name'
    a <- noNegative age'
    w <- noNegative weight'
    weightCheck $ Cow n a w

--

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
    if even i
    then Just (i + 1)
    else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomthing' n = do
    a <- f n
    b <- g a
    c <- h b
    return (a, b,c )

-- years ago
type Founded = Int

-- number of programmers
type Coders = Int

data SoftwareShop =
    Shop {
      founded :: Founded
    , programmers :: Coders
    } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

-- Tho, many programmers *are* negative.
validateCoders
    :: Int
    -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware
    :: Int
    -> Int
    -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers


data Sum a b =
    First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second $ f b

instance (Semigroup a, Semigroup b) => Semigroup (Sum a b) where
    (<>) (First a) (First b) = First (a <> b)
    (<>) (First a) (Second b) = First a
    (<>) (Second a) (First b) = First b
    (<>) (Second a) (Second b) = Second (a <> b)

instance (Semigroup a) =>  Applicative (Sum a) where 
    pure = Second
    (<*>) (First f) (First a) = First (f <> a)
    (<*>) (First f) (Second b) = First f
    (<*>) (Second f) (First a) = First a
    (<*>) (Second f) (Second b) = Second $ f b

instance (Semigroup a) => Monad (Sum a) where
    return = pure
    (>>=) (First a) f = First a
    (>>=) (Second b) f = f b

-- Monad Identity Laws
--
-- 
-- 
-- right identity
-- m >>= return = m
--
-- left identity
-- return x >>= f = f x
--
--
-- Associativity
--
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
--


main :: IO ()
main = do
    putStrLn "Monad >>" >> putStrLn "Hello, " >> putStrLn "World!" 
    putStrLn "Applicative *>" *> putStrLn "Hello, " *> putStrLn "World!"
