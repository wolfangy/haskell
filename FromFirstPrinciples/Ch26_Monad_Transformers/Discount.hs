module Discount where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

data Config = Config {
        discountRate :: Float,
        currencySym :: String
        }

appConfig = Config 10 "CAD"

discount :: Float -> Reader Config Float
discount amt = do
    discountRate' <- reader discountRate 
    return $ (amt * (1.0 - (discountRate' / 100)))

display :: Float -> Reader Config String
display amt = do
    currencySym' <- reader currencySym
    return (currencySym' ++ " " ++ (show amt))

discountT :: Float -> ReaderT Config (Writer String) Float
discountT amt = do
    discountRate' <- asks discountRate
    let discounted = amt * (1 - discountRate' / 100)
    lift $ tell $ " > Discount " ++ (show amt) ++ " = " ++ (show discounted) ++ "\n"
    return discounted

displayT :: Float -> ReaderT Config (Writer String) String
displayT amt = do
    currencySym' <- asks currencySym
    lift $ tell " > Displaying\n" 
    return (currencySym' ++ " " ++ (show amt))

type App = ReaderT Config (Writer String)

discountApp :: Float -> App Float
discountApp = discountT

displayApp :: Float -> App String
displayApp = displayT

doApp :: App a -> (a, String)
doApp app = runWriter $ runReaderT app appConfig

main = do 
    putStrLn $ runReader doDoubleDiscount appConfig
    print $ runWriter $ runReaderT doDoubleDiscountT appConfig
    where
        doDoubleDiscount = discount 100 >>= discount >>= display
        doDoubleDiscountT = discountT 100 >>= discountT >>= displayT
