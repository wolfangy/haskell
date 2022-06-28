{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Foldable

import Control.Monad.Writer

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type SQL = Text

data ErrorMsg = WrongFormat Int Text
    deriving Show

genSQL :: Text -> Writer [ErrorMsg] SQL
genSQL txt = T.concat <$> traverse processLine (zip [1..] $ T.lines txt)

processLine :: (Int, Text) -> Writer [ErrorMsg] SQL
processLine (i, T.splitOn ":" -> [s1, s2]) = pure $ genInsert i s1 s2
processLine (i, s) = tell [WrongFormat i s] >> pure ""

genInsert :: Int -> Text -> Text -> Text
genInsert line s1 s2 = 
    "-- Line " <> (T.pack . show $ line) <> ":\n" <>
     "INSERT INTO items VALUES ('" <> s1 <> "', '" <> s2 <> "');\n"

testData :: Text
testData = "Pen:Bob\nMax\nGlass:Mary\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL :: IO ()
testGenSQL = do
    let (sql, errors) = runWriter $ genSQL testData
    TIO.putStrLn "SQL:"
    TIO.putStr sql
    TIO.putStrLn "Errors:"
    traverse_ print errors

main :: IO()
main = testGenSQL