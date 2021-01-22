module Ch12_Ex where

import Data.Char

-- String processing
--
append :: a -> Maybe [a] -> Maybe [a]
append _ Nothing = Nothing
append x (Just xs) = Just (x:xs)

notThe :: String -> Maybe String
notThe [] = Just []
notThe (x:[]) = Just [x]
notThe rest@(x:y:[]) = Just rest
notThe (x:y:z:rest)
    | (x == 't' || x == 'T')
        && (y == 'h' || y == 'H')
        && (z == 'e' || z == 'E') = Nothing
    | otherwise = append x (notThe (y:z:rest))

isThe :: String -> Bool
isThe str 
    | notThe str == Nothing = True
    | otherwise = False

words' :: String -> [String]
words' str = foldr it [] str
    where
        it a [] = [[a]]
        it a b@(x:xs)
            | a == '\n' = if x == [] then xs else b
            | 'a' <= a && a <= 'z' = (a : x) : xs 
            | 'A' <= a && a <= 'Z' = (a : x) : xs
            | '0' <= a && a <= '9' = (a : x) : xs
            | a == ' ' && x == [] = b
            | otherwise = [] : [a] : b

isWord :: String -> Bool
isWord [] = False
isWord (a:_) = 'a' <= a && a <= 'z'
            || 'A' <= a && a <= 'Z'
            || '0' <= a && a <= '9'

onlyWords :: String -> [String]
onlyWords  = (filter isWord) . words'

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel x = (x `elem` vowels) || x `elem` (map toUpper vowels)

hasValue :: Maybe a -> Bool
hasValue Nothing = False
hasValue (Just a) = True

-- 1.
--
replaceThe :: String -> String
replaceThe str = concat . (map replace) . words' $ str
    where
        replace word
            | notThe word == Nothing = "a"
            | otherwise = word

wordStartByVowel :: String -> Bool
wordStartByVowel [] = False
wordStartByVowel (x:_) = isVowel x

-- 2.
--

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = count (onlyWords str)
    where
        count [] = 0
        count (x:[]) = 0
        count (x:y:zs)
            | isThe x && wordStartByVowel y = 1 + (count zs)
            | otherwise = count (y:zs)

-- 3.
--
countVowels :: String -> Int
countVowels = length . (filter isVowel)

-- Validate the word
--
newtype Word' = Word' String deriving (Eq, Show)


mkWord :: String -> Maybe Word'
mkWord str = let vowelCount = countVowels str
                 wordLength = length str 
             in
                case (wordLength - vowelCount) >= vowelCount of
                    True -> Just (Word' str)
                    False -> Nothing

-- It's only Natural
--

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0 :: Integer
natToInteger (Succ Zero) = 1 :: Integer
natToInteger (Succ (Succ x)) = (1 :: Integer) + (natToInteger (Succ x))

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0 = Nothing
    | i == 0 = Just Zero 
    | otherwise = Just (toNat i)
    where 
        toNat :: Integer -> Nat
        toNat 1 = Succ Zero
        toNat i = Succ (toNat (i - 1))

-- Small library for Maybe
--

-- 1.
--
isJust :: Maybe a -> Bool
isJust = hasValue

isNothing :: Maybe a -> Bool
isNothing = not . hasValue

-- 2.
--
maybee :: b -> (a -> b) -> Maybe a -> b
maybee d _ Nothing = d
maybee _ fn (Just val) = fn val

-- 3.
--
fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just x) = x

fromMaybe' :: a -> Maybe a -> a
fromMaybe' d m = maybee d id m

-- 4.
--
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = maybee [] (: [])

-- 5.
--
catMaybes :: [Maybe a] -> [a]
catMaybes = concat . (map maybeToList)

catMaybes' :: [Maybe a] -> [a]
catMaybes' = foldr appendJust []
    where
        appendJust Nothing b = b
        appendJust (Just a) b = a : b

-- 6.
--
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = let rs = catMaybes ms in
            case length ms == length rs of
                True -> Just rs
                False -> Nothing

-- Small library for Either
--

-- 1.
--
lefts' :: [Either a b] -> [a]
lefts' = foldr appendLeft []
    where
        appendLeft (Right _) b = b
        appendLeft (Left l) b = l : b

-- 2.
--
right' :: [Either a b] -> [b]
right' = foldr appendRight []
    where
        appendRight (Left _) b = b
        appendRight (Right a) b = a : b

-- 3.
--
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (ls, rs)
    where 
        ls = lefts' xs
        rs = right' xs

-- 4.
--
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' fn (Right r) = Just (fn r)

-- 5.
--
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fn _ (Left l) = fn l
either' _ fn (Right r) = fn r


-- 6.
--
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' fn e = either' (\_ -> Nothing) (Just . fn) e

-- Unfolds
--
myIterate :: (a -> a) -> a -> [a]
myIterate fn a = (fn a) : (myIterate fn a)

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' fn x = case fn x of
    Nothing -> []
    Just (a, b) -> a : (unfoldr' fn b)

unfoldr'' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr'' fn x = ret $ fn x
    where
        ret Nothing = []
        ret (Just (a, b)) = a : (unfoldr'' fn b)

iterate'' :: (a -> a) -> a -> [a]
iterate'' fn = unfoldr' (\x -> Just (fn x, fn x))

-- Other than a list
--

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord)

instance (Show a) => Show (BinaryTree a) where
    show tree = go tree 0
        where
            go Leaf _ = ""
            go (Node l v r) lvl = go l (lvl + 1) ++ (toSpace lvl) ++ "[" ++ show v ++ "]\n"++ go r (lvl + 1)

            toSpace 0 = ""
            toSpace n = "      " ++ toSpace (n-1)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold fn a = case fn a of
    Nothing -> Leaf
    (Just (al, b, ar)) -> Node (unfold fn al) b (unfold fn ar)

treeBuilder :: Integer -> BinaryTree Integer
treeBuilder max = unfold (\a -> if a == max then Nothing else Just (a+1, a, a+1)) 0
