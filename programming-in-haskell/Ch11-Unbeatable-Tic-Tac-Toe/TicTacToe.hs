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

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
    if valid g i then [chop size (xs ++ [p] ++ ys)] else []
    where
        (xs, B:ys) = splitAt i (concat g)

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

-- 11.8 Game Tree

data Tree a where
    Node :: a -> [Tree a] -> Tree a
    deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p =
    Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
    | won g = []
    | full g = []
    | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

-- 11.4 Displaying a grid

putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
    where
        bar = [replicate ((size * 4) - 1) '-']

-- rs = map showRow [[B, O, O], [O, X, O], [X, X, X]] ==> rs = [["   |   |   ", "   | O | O ", "   |   |   "],
--                                                              ["   |   |   ", " O | X | O ", "   |   |   "],
--                                                              ["   |   |   ", " X | X | X ", "   |   |   "]]
--
-- bs = interleave bar  ==> bs = [  ["   |   |   ", "   | O | O ", "   |   |   "],
--                                  ["-----------"], 
--                                  ["   |   |   ", " O | X | O ", "   |   |   "],
--                                  ["-----------"],
--                                  ["   |   |   ", " X | X | X ", "   |   |   "] ]
--
-- cs = concat bs       ==> cs = [  "   |   |   ",
--                                  "   | O | O ",
--                                  "   |   |   ",
--                                  "-----------",
--                                  "   |   |   ",
--                                  " O | X | O ",
--                                  "   |   |   ",
--                                  "-----------",
--                                  "   |   |   ",
--                                  " X | X | X ",
--                                  "   |   |   "]
--
-- us = unlines cs      ==> us = "   |   |   \n   | O | O \n   |   |   \n-----------\n   |   |   \n O | X | O \n   |   |   \n-----------\n   |   |   \n X | X | X \n   |   |   \n"


showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
        beside = foldr1 (zipWith (++))
        bar    = replicate 3 "|"

-- e.g. 
-- player = [X, O, O]
-- let ps = map shoRow player           ==> ps = [["   "," X ","   "],["   "," O ","   "],["   "," O ","   "]]
-- let bs = interleave bar ps           ==> bs = [["   "," X ","   "],["|","|","|"],["   "," O ","   "],["|","|","|"],["   "," O ","   "]]
-- let fs = fordr1 (zipWith (++)) bs    ==> fs = ... zipWith (++) ["   |", " X |", "   |"] ["   ", " O ", "   "] ...
--                                      ==> fs = ... zipWith (++) ["   |   ", " X | O ", "   |   "] ["|", "|", "|"] ...
--                                      ==> fs = ... zipWith (++) ["   |   |", " X | O |", "   |   |"] ["   ", " O ", "   "]
--                                          fs = ["   |   |   "," X | O | O ","   |   |   "]
--

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

getNat :: String -> IO Int
getNat prompt = do
    putStr prompt
    xs <- getLine
    if xs /= [] && all isDigit xs then
        return (read xs)
    else
        --do
            putStrLn "Error: Invalid number" >>
            getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
    cls
    goto (1, 1)
    putGrid g
    run' g p

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run' :: Grid -> Player -> IO ()
run' g p 
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g = putStrLn "It's a draw!\n"
    | otherwise = do
        i <- getNat (prompt p)
        case move g i p of
            [] -> do
                putStrLn "ERROR: Invalid move"
                run' g p
            [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "


-- Utilities:

interleave :: a -> [a] -> [a]
interleave _ []     = []
interleave n [x]    = [x]
interleave n (x:xs) = x : n : interleave n xs

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)