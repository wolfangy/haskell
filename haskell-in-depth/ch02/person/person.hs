{-# LANGUAGE StandaloneDeriving #-}

module Main where

--import Data.String(IsString, fromString)
import TextShow(TextShow, showb, fromString, printT)

data Person where
    Person :: String -> Maybe Int -> Person

homer :: Person
homer = Person "Homer Simpson" (Just 39)

spj :: Person
spj = Person "Simon Peyton Jones" Nothing

-- instance IsString Person where
--     fromString name = Person name Nothing

deriving instance Show Person

deriving instance Read Person

deriving instance Eq Person

instance TextShow Person where
    showb (Person name Nothing) =
        fromString name
    showb (Person name (Just age)) =
        fromString name <> " (" <> showb age <> ")"

main :: IO ()
main = do
    printT homer
    printT spj