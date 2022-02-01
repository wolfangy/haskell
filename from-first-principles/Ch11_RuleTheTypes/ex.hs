module Ch11_Ex where

import Data.Char

-- Vigenere Cipher
--
-- keyword: ALLY
-- message: meet at dawn
-- encoding:
-- MEET AT DAWN
-- ALLY AL LYAL
-- MPPR AE OYWY
--
expand :: String -> String -> String
expand keys targets = go targets keys
    where
        go [] _ = []
        go target [] = go target keys
        go (t:ts) rotate@(r:rs)
            | t == ' ' = ' ' : (go ts rotate)
            | otherwise = r : (go ts rs)


offset :: Char -> Int
offset ch =
    if 'a' <= ch && ch <= 'z'
    then (ord ch) - (ord 'a')
    else (ord ch) - (ord 'A')

shift :: Char -> Char -> Char
shift t by 
    | t == ' ' = ' '
    | otherwise = chr $ calc t by
    where
        calc a b = 
            if 'a' <= a && a <= 'z' then (offset a + offset b) `mod` 26 + (ord 'a')
            else (offset a + offset b) `mod` 26 + (ord 'A')

unshift :: Char -> Char -> Char
unshift t by
    | t == ' ' = ' '
    | otherwise = chr $ calc t by
    where
        calc a b =
            if 'a' <= a && a <= 'z' then (offset a - offset b) `mod` 26 + (ord 'a')
            else (offset a - offset b) `mod` 26 + (ord 'A')

vigenere :: String -> String -> String
vigenere key str = encode (expand key str) str
    where
        encode :: String -> String -> String
        encode _ [] = []
        encode (k:ks) (t:ts) = (shift t k) : encode ks ts

keyword = "ALLY"
message = "meet at dawn"

unVigenere key str = decode (expand key str) str
    where
        decode :: String -> String -> String
        decode _ [] = []
        decode (k:ks) (t:ts) = (unshift t k) : decode ks ts

-- AS-Patterns
--
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf (x:_) [] = False
isSubseqOf [] _ = True
isSubseqOf sub@(x:xs) (y:ys)
    | x == y = isSubseqOf xs ys
    | otherwise = isSubseqOf sub ys 

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = [(w, capital w) | w <- (words' s)]

words' :: String -> [String]
words' str = foldr it [] str
    where
        it a [] = [[a]]
        it a b@(x:xs) 
            | a == '\n' = if x == [] then xs else b
            | 'a' <= a && a <= 'z' || 'A' <= a && a <= 'Z' = (a : x) : xs
            | otherwise = if x == [] then b else [] : b
        trim [] = []
        trim ([]:xs) = xs
        trim xs@(x:_) = xs

capital [] = []
capital (x:xs) = (toUpper x):xs

-- Language exercies
--
-- 1. 
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord all@(x:xs)
    | 'a' <= x && x <= 'z' = (toUpper x) : xs
    | 'A' <= x && x <= 'Z' = (toLower x) : xs
    | otherwise = all

words'' :: String -> [String]
words'' = foldr it []
    where
        it a [] = [[a]]
        it a b@(x:xs)
            | 'a' <= a && a <= 'z' || 'A' <= a && a <= 'Z' = (a : x) :xs
            | otherwise = []:[a]:b

capitalizeParagraph :: String -> String
capitalizeParagraph p = concat $ foldr ((:).capitalizeWord) [] (words'' p)

-- Phone exercies
--

data Key = DigitKey Digit [Digit] deriving (Eq, Show)
data DaPhone = DaPhone [Key] deriving (Eq, Show)

key1 = DigitKey '1' ['1']
key2 = DigitKey '2' ['A', 'B', 'C', '2']
key3 = DigitKey '3' ['D', 'E', 'F', '3']
key4 = DigitKey '4' ['G', 'H', 'I', '4']
key5 = DigitKey '5' ['J', 'K', 'L', '5']
key6 = DigitKey '6' ['M', 'N', 'O', '6']
key7 = DigitKey '7' ['P', 'Q', 'R', 'S', '7']
key8 = DigitKey '8' ['T', 'U', 'V', '8']
key9 = DigitKey '9' ['W', 'X', 'Y', 'Z', '9']
keyS = DigitKey '*' ['^', '*']
key0 = DigitKey '0' ['+', ' ', '0']
keyP = DigitKey '#' ['.', ',', '#']

type Digit = Char
type Letter = Char

type Press = Int

phone :: DaPhone
phone = DaPhone [
    key1, key2, key3,
    key4, key5, key6,
    key7, key8, key9,
    keyS, key0, keyP]


convo :: [String]
convo = [
    "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

tapUpper :: (Digit, Int)
tapUpper = ('*', 1)

reverseTaps :: DaPhone -> Char -> [(Digit, Int)]
reverseTaps (DaPhone keys) ch = scan keys ch
    where
        scanKey :: [Digit] -> Digit -> Int -> Int
        scanKey [] ch indx = -1
        scanKey (k:ks) ch indx = if k == ch then indx else scanKey ks ch (indx + 1)

        scan :: [Key] -> Digit -> [(Digit, Int)]
        scan all@((DigitKey k val):ks) ch = 
            let result = scanKey val (toUpper ch) 0 
                needTapUpper = isUpper ch in
                if result >= 0
                then
                    if needTapUpper
                    then tapUpper : [(k, result + 1)]
                    else [(k, result + 1)]
                else scan ks ch

toKeyPress :: (Char, Int) -> String
toKeyPress (ch, 0) = []
toKeyPress (ch, count) = ch : (toKeyPress (ch, count -1))

cellPhoneDead :: DaPhone -> String -> [(Digit, Press)]
cellPhoneDead phone str = concat . (map tab) $ str
    where tab = reverseTaps phone

fingerTaps :: [(Digit, Press)] -> Press
fingerTaps = length . concat . (map toKeyPress) 


-- Hutton's Razor
--

data Expr = 
    Lit Integer
    | Add Expr Expr
    deriving (Eq, Show)


eval :: Expr -> Integer
eval (Lit val) = val
eval (Add left right) = (eval left) + (eval right)


printExpr :: Expr -> String
printExpr (Lit val) = show val
printExpr (Add left right) = (printExpr left) ++ " + " ++ (printExpr right)

expr1 = Add (Lit 9001) (Lit 1)
expr2 = Add expr1 (Lit 20001)
expr3 = Add (Lit 1) expr2
