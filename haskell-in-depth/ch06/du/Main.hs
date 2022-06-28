module Main where

import Data.Text.IO as TIO
import TextShow

import App

import FileCounter
import DirTree

buildEntries :: Builder -> (e -> Builder) -> [e] -> Builder
buildEntries title entryBuilder entries =
    unlinesB $ title : map entryBuilder entries

treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp, n) = fromString indent <> fromString fp
    where
        indent = replicate (2 * n)  ' '

tabEntryBuilder :: TextShow s => (FilePath, s) -> Builder
tabEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp

work :: AppConfig -> IO ()
work config = do
    (_, dirs) <- runMyApp dirTree config ()
    (_, counters) <- runMyApp fileCount config ()
    let report = toText $
            buildEntries "Directory tree: " treeEntryBuilder dirs
            <> buildEntries "File counter:   " tabEntryBuilder counters
    TIO.putStr report

main :: IO ()
main = undefined