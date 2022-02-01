module Lib (caesar, uncaesar, vigenere, unvigenere) where

import Data.Char

shift :: Int -> Char -> Char
shift 0 input = input
shift count ch
    | ch >= 'a' && ch <= 'z' = chr (mod ((ord ch) - smA + count) 26 + smA)
    | ch >= 'A' && ch <= 'Z' = chr (mod ((ord ch) - bgA + count) 26 + bgA)
    | ch == ' ' = ' '
    | otherwise = chr . (+ count) .ord $ ch
    where
        smA = ord 'a'
        smZ = ord 'z'
        bgA = ord 'A'
        bgZ = ord 'Z' 

caesar :: Int -> String -> String
caesar count message = map (shift count) message

uncaesar :: Int -> String -> String
uncaesar count message = map unshift message
    where unshift = shift (- count)

--------------------------------------------------------------------------------

-- ALLY
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

shift' :: Char -> Char -> Char
shift' t by 
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
        encode (k:ks) (t:ts) = (shift' t k) : encode ks ts

keyword = "ALLY"
message = "meet at dawn"

unvigenere key str = decode (expand key str) str
    where
        decode :: String -> String -> String
        decode _ [] = []
        decode (k:ks) (t:ts) = (unshift t k) : decode ks ts

