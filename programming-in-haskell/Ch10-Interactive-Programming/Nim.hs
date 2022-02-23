module Nim where

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4 .. 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num =
  [ if row == r then n - num else n
    | (r, n) <- zip [1..] board
  ]

putRow' :: Int -> Int -> IO ()
putRow' row num = putStr (show row) >> putChar ':' >> putStars num
    where putStars num = putStrLn . map (const '*') $ [1..num]

putRow :: Int -> Int -> IO ()
putRow row num = do
        putStr (show row)
        putStr ": "
        putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard board = putBoardByRow board 1
    where
        putBoardByRow [] row = return ()
        putBoardByRow (x:xs) row = putRow row x >> putBoardByRow xs (row + 1)

getDigit :: String -> IO Int
getDigit prompt = do
    putStrLn prompt
    x <- getLine
    if isDigit x then return (digitToInt x)
    else
        do
            putStrLn "Error: Invalid digit"
            getDigit prompt
    where
        isDigit :: String -> Bool
        isDigit ch = ch `elem` map (:[]) ['0', '1'..'9']
        digitToInt ch = read ch :: Int

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player =
    do
        newline
        putBoard board
        if finished board then
            do
                newline
                putStr "Player "
                putStr (show (next player))
                putStrLn " wins"
        else
            do
                newline
                putStr "Player "
                putStrLn (show player)
                row <- getDigit "Enter a row number:"
                num <- getDigit "Enter stars to remove:"
                if valid board row num then
                    play (move board row num) (next player)
                else
                    do
                        newline
                        putStrLn "Error: Invalid move!"
                        play board player

nim :: IO ()
nim = play initial 1
