module BinaryStringTransmitter where

import Data.Char ( ord, chr )

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

-- iterate produces an infinite list by applying a function an increasing number of times to a value
-- iterate f x = [x, f x, f (f x), f (f (f x)), ...]


bin2int' :: [Bit] -> Int 
bin2int' = foldr (\a b -> a + 2 * b) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

make :: Int -> [Bit] -> [Bit]
make n bits
    | length bits > n  = error "Bits loss"
    | otherwise        = take n (bits ++ repeat 0)

make8' = make 8

make9 xs = 
  where 
    bits = make 8 bits

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop n bits = take n bits: chop n (drop n bits)

chop8' = chop 8

chop9 = chop 9

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

encodeWithParity :: String -> [Bit]
encodeWithParity =

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String 
transmit = decode . channel . encode
