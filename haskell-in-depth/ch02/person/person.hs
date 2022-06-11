{-# LANGUAGE StandaloneDeriving #-}

module Person where

import Data.String(IsString, fromString)

data Person where
    Person :: String -> Maybe Int -> Person

homer :: Person
homer = Person "Homer Simpson" (Just 39)

spj :: Person
spj = Person "Simon Peyton Jones" Nothing

instance IsString Person where
    fromString name = Person name Nothing

deriving instance Show Person

deriving instance Read Person

deriving instance Eq Person
