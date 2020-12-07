{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random

sampleBytes :: B.ByteString
sampleBytes = "Hello"

sampleString :: String
sampleString = BC.unpack sampleBytes

bcInt :: BC.ByteString
bcInt = "6"
sampleBCInt :: Int
sampleBCInt = (read . BC.unpack) bcInt

randomChar :: IO Char
randomChar = do
    randomInt <- randomRIO(0, 255)
    return (toEnum randomInt)