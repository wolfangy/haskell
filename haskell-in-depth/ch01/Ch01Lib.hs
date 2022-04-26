module Ch01Lib where

import Data.Char
import Data.List (group, sort)
import Data.Text(Text)
import qualified Data.Text as T

type Entry = (Text, Int)

type Vocabulary = [Entry]

extractVocab :: Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
    where
        ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
        buildEntry xs@(x:_) = (x, length xs)
        cleanWord = T.dropAround (not . isLetter)