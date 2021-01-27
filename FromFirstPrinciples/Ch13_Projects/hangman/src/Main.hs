module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList $ (filter allCh) . (filter gameLength) $ aw
    where
        allCh = all (\x -> x `elem` (['a'..'z'] ++ ['A'..'Z']))
        gameLength w =
            let l = length (w :: String)
            in      l >= minWordLength
                &&  l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, ((length wl) - 1))
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

extraChance :: Int
extraChance = 2

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
    show (Puzzle word discovered guessed count) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ "  |  Guess so far: " ++ (guessedSoFar guessed)
        ++ "  |  Trials left: " ++ (show left) ++ "/" ++ (show total) 
        where
            total = (length word) + extraChance
            left = total - count
            guessedSoFar [] = "N.A"
            guessedSoFar a@(x:_) = intersperse ' ' a

freshPuzzle :: String -> Puzzle
freshPuzzle p = Puzzle p (map (const Nothing) p) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle v _ _ _) ch = ch `elem` v

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) ch = ch `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Nothing) = '_'
renderPuzzleChar (Just ch) = ch

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s count) c =
    Puzzle word newFilledInSoFar (c:s) newCount
    where
        zipper :: Char -> Char -> Maybe Char -> Maybe Char
        zipper guessed wordChar guessChar =
            if wordChar == guessed then Just wordChar
            else guessChar

        countOfJust = length . (filter isJust)
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar
        newCount = if (countOfJust newFilledInSoFar) == (countOfJust filledInSoFar)
                   then count + 1
                   else count

handleGuess :: Puzzle -> Char -> IO Puzzle 
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True)   -> do putStrLn "You already guessed that character, pick something else!"
        (True, _)   -> do putStrLn "This character was in the word, filling in the word accordingly"
        (False, _)  -> do putStrLn "This character wasn't in the word, try again"
    return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed count) = 
    if count >= ((length wordToGuess) + extraChance)
    then do
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
        exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
    if all isJust filledInSoFar
    then do 
        putStrLn "You win!"
        exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        (':':'q':[]) -> exitSuccess
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

