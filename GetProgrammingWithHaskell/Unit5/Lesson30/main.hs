import qualified Data.Map as Map

double :: Int -> Int
double x = 2 *x

convertString :: String -> Int
convertString str = read str

doubleInput :: IO ()
doubleInput = 
    getLine >>= (print <$> show <$> double <$> convertString) -- function is also Functor

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          ,(2,"KINGinYELLOW")
                          ,(3,"dagon1997")
                          ,(4,"rcarter1919")
                          ,(5,"xCTHULHUx")
                          ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                         ,("KINGinYELLOW",15000)
                         ,("dagon1997",300)
                         ,("rcarter1919",12)
                         ,("xCTHULHUx",50000)
                         ,("yogSOThoth",150000)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits id = Map.lookup id creditsDB

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = (lookupUserName id) >>= lookupCredits

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1)
                         ,(1002,2)
                         ,(1003,3)
                         ,(1004,4)
                         ,(1005,5)
                         ,(1006,6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = (lookupGamerId id) >>= lookupUserName >>= lookupCredits

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble i = print (i * 2)

readThenPrintDouble :: IO ()
readThenPrintDouble = readInt >>= printDouble

echoVerbose :: IO ()
echoVerbose = (putStrLn "Enter a String for echo") >> getLine >>= putStrLn

askForName :: IO ()
askForName = putStrLn "Enter your name: "

helloName :: IO ()
helloName = askForName 
    >> getLine 
    >>= (\name -> return ("Hello, " ++ name)) 
    >>= putStrLn
    
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM fn m = m >>= (\n -> return (fn n))

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp fn m =  fn >>= (\f -> m >>= (\x -> return (f x)))

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just val) fn = fn val

maybeToString :: Show a => Maybe (a -> String)
maybeToString = pure show