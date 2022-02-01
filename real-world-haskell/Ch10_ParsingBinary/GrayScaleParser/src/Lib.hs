{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace, chr, isDigit)
import Data.Int (Int64)
import Data.Word

data Greymap = Greymap {
          greyWidth :: Int
        , greyHeight :: Int
        , greyMax :: Int
        , greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h
                            ++ " " ++ show m

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
} deriving (Show)

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}

instance Functor Parse where
    fmap f (Parse run) = Parse $
        \state -> case run state of
            Left err -> Left err
            Right (var, newState) -> Right (f var, newState)

byteStringPack = L8.pack
byteStringUnCons = L8.uncons

identity :: a -> Parse a
identity a = Parse (\s -> Right(a, s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err -> Left  err
        Right (result, _) -> Right result

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset }

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> nextParserFac = Parse chaindParser
    where
        chaindParser initState =
            case runParse firstParser initState of
                Left errMessage -> Left errMessage
                Right (firstResult, newState) ->
                    runParse (nextParserFac firstResult) newState

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

parseByte :: Parse Word8
parseByte =
    getState ==>
        \initState ->
            case L.uncons (string initState) of
                Nothing -> bail "no more input"
                Just (byte, remainder) ->
                    putState newState ==> \_ ->
                        identity byte
                    where
                        newState = initState {
                            string = remainder,
                            offset = newOffset
                            }
                        newOffset = offset initState + 1

parseP5' :: L.ByteString  -> Maybe (Greymap, L.ByteString)
parseP5' s =
    matchHeader (L8.pack "P5") s >>=
        \rest -> skipSpace((), s) >>=
            (getNat . snd) >>=
                skipSpace >>=
                    \(width, s) ->  getNat s >>=
                        skipSpace >>=
                            \(height, s) -> getNat s >>=
                                \(maxGrey, s) -> getBytes 1 s >>=
                                    (getBytes (width * height) . snd) >>=
                                        \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString )
parseP5 s = do
    content <- matchHeader (L8.pack "P5") s
    v <- skipSpace ((), content)
    widthContent@(width, restOfWidth) <- getNat . snd $ v
    v <- skipSpace widthContent
    heightAndContent@(height, restOfHeight) <- getNat. snd $ v
    v <- skipSpace heightAndContent
    (maxGray, restOfMaxGrey) <- getNat . snd $ v
    v <- getBytes 1 restOfMaxGrey
    (bitmap, rest) <- getBytes (width * height) . snd $ v
    return (Greymap width height maxGray bitmap, rest)


skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString )
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

matchHeader :: L.ByteString  -> L.ByteString  -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
    Nothing -> Nothing
    Just (num, rest)
        | num <= 0 -> Nothing
        | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString )
getBytes n str = let count            = fromIntegral n
                     both@(prefix, _) = L.splitAt count str
                in
                    if L.length prefix < count
                        then Nothing
                        else Just both

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined;

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined;

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = fmap fst . L.uncons . string <$> getState

peekChar :: Parse (Maybe Char)
--peekChar = (fmap . fmap) w2c peekByte
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==>
                \mp -> if mp == Just True
                        then parseByte ==> \b ->
                            (b:) <$> parseWhile p
                        else identity []

parseWhileVerbose :: (Word8 -> Bool) -> Parse [Word8]
parseWhileVerbose p =
    peekByte ==>
    \mc ->
        case mc of
            Nothing -> identity []
            Just c
                | p c -> parseByte ==> \b ->
                            parseWhileVerbose p ==> \bs ->
                                identity (b : bs)
                | otherwise -> identity []


parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==>
    \digits ->
        if null digits
        then bail "no more input"
        else let n = read digits
            in if n < 0
                then bail "integer overflow"
                else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> const f

skipSpaces :: Parse()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool  -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
        let n' = fromIntegral n
            (h, t) = L.splitAt n' (string st)
            st' = st { offset = offset st + L.length h, string = t }
        in
            putState st' ==>&
            assert (L.length  h == n') "end of input" ==>&
            identity h

parseRawPGM :: Parse Greymap
parseRawPGM = 
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap -> 
        identity (Greymap width height maxGrey bitmap)
    where
        notWhite :: Char -> Bool
        notWhite c = c `notElem` ['\t', '\n', ' ']