module Main where

import Lib
import Ex9 (possibleExprs, correctExprs)


nums :: [Int]
nums = [1, 3, 7, 10, 25, 50]

answer :: [Int]
answer = [765]
 
main :: IO()
main = do
    print $ possibleExprs nums
    print $ correctExprs nums
    -- print (solutions' [1, 3, 7, 10, 25, 50] 765)
