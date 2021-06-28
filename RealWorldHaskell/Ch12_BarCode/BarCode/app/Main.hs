module Main where

import Lib

indicesTuple4 (a,b,c,d) i
    | i == 0 = a
    | i == 1 = b
    | i == 2 = c
    | i == 3 = d

main :: IO ()
main = undefined

