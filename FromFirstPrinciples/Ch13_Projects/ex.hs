module Ch13_Ex where

import Data.Char
import Control.Monad
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

-- 2.
-- 3.
--

onlyText :: Char -> Bool
onlyText = (`elem` (['a'..'z'] ++ ['A'..'Z']))

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case ((process line1) == (reverse . process $ line1)) of
        True  -> putStrLn "It's a palindrome!"
        False -> do
            putStrLn "Nope!"
            exitSuccess
            return ()
    where
        process :: String -> String
        process = (map toLower) . (filter onlyText)

-- 4.
--

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == ""            = Left NameEmpty
    | not (age > 0)         = Left AgeTooLow
    | otherwise             = Left $ PersonInvalidUnknown $ "Name was: " 
                                ++ show name 
                                ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStrLn "Name: "
    name <- getLine

    putStrLn "Age: "
    ageStr <- getLine

    case readMaybe ageStr :: Maybe Integer of
        (Just age) -> output $ mkPerson name age 
        Nothing -> putStrLn "Age is not good"
    where
        output :: Either PersonInvalid Person -> IO ()
        output (Right p) = putStrLn $ "Yay! Successfully got a person: " ++ show p
        output (Left NameEmpty) = putStrLn $ "No..." ++ show NameEmpty
        output (Left AgeTooLow) = putStrLn $ "No..." ++ show AgeTooLow
        output (Left a@(PersonInvalidUnknown s)) = putStrLn $ "No..." ++ show a
