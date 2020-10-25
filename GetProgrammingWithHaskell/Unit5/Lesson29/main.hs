maybeSix :: Maybe Int
maybeSix = pure 6

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

boxPrize' :: [Int]
boxPrize' = [10, 50]

totalPrize' :: [Int]
totalPrize' = pure (*) <*> doorPrize <*> boxPrize'

primeToN :: Integer -> [Integer]
primeToN n = filter isNotComposite twoThroughN
    where
        twoThroughN = [2..n]
        half = (round . sqrt) (fromInteger n :: Double)
        twoThroungHalf = [2..half]
        composite = pure (*) <*> twoThroungHalf <*> twoThroughN
        isNotComposite = not . (`elem` composite)

data User = User {
     name :: String
   , gamerId :: Int
   , score :: Int
} deriving Show

testNames :: [String]
testNames = ["John Smith"
            ,"Robert'); DROP TABLE Students;--"
            ,"Christina NULL"
            ,"Randall Munroe"]

testIds :: [Int]
testIds = [1337
          ,0123
          ,999999]

testScores :: [Int]
testScores = [0
             ,100000
             ,-99999]

testData :: [User]
testData = pure User 
    <*> testNames
    <*> testIds
    <*> testScores

-- Q29.1
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap fn ctx = pure fn <*> ctx

-- Q29.2
exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

-- Q29.3
beerLeft :: [Int]
beerLeft = pure (-) <*> [6, 12] <*> [4]

beerNeed :: [Int]
beerNeed = pure (*) <*> [3, 4] <*> [3, 4]

beerToBuy :: [Int]
beerToBuy = pure (-) <*> beerNeed <*> beerLeft