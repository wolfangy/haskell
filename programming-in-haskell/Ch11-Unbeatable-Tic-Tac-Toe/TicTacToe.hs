{-# LANGUAGE GADTs #-}

module TicTacToe where

import Data.Char
import Data.List
import System.IO

-- 11.2 Basic declarations

size :: Int
size = 3

type Grid = [[Player]]

data Player where
    O :: Player
    B :: Player
    X :: Player
    deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

-- 11.3 Grid utilities

empty :: Grid
empty = replicate size $ replicate size B

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if os < xs then O else X
    where
        os = length . filter (== O) $ ps
        xs = length . filter (== X) $ ps
        ps = concat g

wins :: Player -> Grid -> Bool
wins player grid = any line (rows ++ cols ++ diagonals)
    where
        line = all (== player)
        rows = grid
        cols = transpose grid
        diagonals = [diag grid, diag (map reverse grid)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g


-- 11.4 Displaying a grid

putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
    where
        bar = [replicate ((size * 4) - 1) '-']


showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
        beside = foldr1 (zipWith (++)) 
        bar    = replicate 3 "|"

-- e.g. 
-- player = [X, O, O]
-- let ps = map shoRow player           ==> ps = [["   "," X ","   "],["   "," O ","   "],["   "," O ","   "]]
-- let bs = interleave bar ps           ==> bs = [["   "," X ","   "],["|","|","|"],["   "," O ","   "],["|","|","|"],["   "," O ","   "]]
-- let fs = fordr1 (zipWith (++)) bs    ==> fs = ["   |   |   "," X | O | O ","   |   |   "]

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave _ []     = []
interleave n [x]    = [x]
interleave n (x:xs) = x : n : interleave n xs