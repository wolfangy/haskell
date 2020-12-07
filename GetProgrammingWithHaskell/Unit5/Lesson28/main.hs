maybeFirstName :: Maybe String
maybeFirstName = Just "Alan"

maybeLastName :: Maybe String
maybeLastName = Just "Turing"

concatName :: Maybe String -> Maybe String -> Maybe String
concatName fstName lstName = (++) <$> fstName <*> lstName


minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

minOfMaybes :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
minOfMaybes a b c = minOfThree <$> a <*> b <*> c

data User = User {
    name :: String
    , gameId :: Int
    , score :: Int
} deriving Show


serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId =  Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

createdUser = User <$> serverUsername <*> serverGamerId <*> serverScore
