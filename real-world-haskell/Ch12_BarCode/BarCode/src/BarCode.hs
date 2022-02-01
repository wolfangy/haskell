module BarCode where

import Data.Char
import Data.Ix
import Data.Array
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Word
import Control.Applicative
import Control.Monad
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Parse

checkDigit ::  (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
    where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
              "0110001", "0101111", "0111011", "0110111", "0001011"]

rightList = map complement <$> leftOddList
    where
        complement '0' = '1'
        complement '1' = '0'

leftEvenList = map reverse rightList

parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, l-1) xs
    where l = length xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String

leftOddCodes    = listToArray leftOddList
leftEvenCodes   = listToArray leftEvenList
rightCodes      = listToArray rightList
parityCodes     = listToArray parityList

encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
        outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
    where
        (left, right) = splitAt 5 rest
        lefties = zipWith leftEncode (parityCodes ! first) left
        righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

outerGuard :: String
outerGuard = "101"

centerGuard :: String
centerGuard = "01010"
