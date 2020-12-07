import qualified Data.Map as Map

maxPairM :: (Monad m, Ord a) => m  (a, a) -> m a
maxPairM pairM = pairM >>= (\(n, m) -> if n > m then return n else return m)

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

printHelloName :: IO()
printHelloName = do
    name <- getLine
    let statement = helloPerson name
    putStrLn statement

echo :: IO ()
echo = do
    input <- getLine
    putStrLn input

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate {
    candidateId :: Int
   , codeReview :: Grade
   , cultureFit :: Grade
   , education :: Degree 
   } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
   where passedCoding = codeReview candidate > B
         passedCultureFit = cultureFit candidate > C
         educationMin = education candidate >= MS
         tests = [passedCoding
                 ,passedCultureFit
                 ,educationMin]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readGrade' :: IO Grade
readGrade' = do
    input <- getLine
    let g = read input :: Grade
    return g

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
   putStrLn "enter id:"
   cId <- readInt
   putStrLn "enter code grade:"
   codeGrade <- readGrade
   putStrLn "enter culture fit grade:"
   cultureGrade <- readGrade
   putStrLn "enter education:"
   degree <- readDegree
   return (Candidate { candidateId = cId
                     , codeReview = codeGrade
                     , cultureFit = cultureGrade
                     , education = degree })

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement = if  passed then "passed" else "failed"
    return statement

candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1)
                           ,(2,candidate2)
                           ,(3,candidate3)]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
   candidate <- Map.lookup cId candidateDB
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement

candidates :: [Candidate]
candidates = [candidate1
             ,candidate2
             ,candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
   candidate <- candidates
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement


assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidateM = do
    candidate <- candidateM
    let result = viable candidate
    let statement = if result
            then "passed"
            else "failed"
    return statement



-- Q31.1 
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

type Pizza = (Double,Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
   where costP1 = costPerInch p1
         costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza " ++
                            "is cheaper at " ++
                            show costSqInch ++
                            " per square inch"
   where costSqInch = costPerInch (size,cost)

pizzaCompare :: IO ()
pizzaCompare = 
    (askPizza "1") >>= 
        (\pizza1 -> (askPizza "2") >>= 
            (\pizza2 -> (\description -> putStrLn description) (describePizza(comparePizzas pizza1 pizza2))))
    where
        askPizza index = putStrLn ("What is size of pizza" ++ index ++ "?")
            >> getLine 
            >>= (\size -> 
                putStrLn ("What is the cost of pizza" ++ index ++ "?")
                >> getLine
                >>= (\cost -> return (read size :: Double, read cost :: Double)))

-- Q31.2
sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.0)]

costData :: Map.Map Int Double
costData = Map.fromList [(1,18.0),(2,16.0)]

maybeMain :: Maybe String
maybeMain = do
   size1 <- Map.lookup 1 sizeData
   cost1 <- Map.lookup 1 costData
   size2 <- Map.lookup 2 sizeData
   cost2 <- Map.lookup 2 costData
   let pizza1 = (size1,cost1)
   let pizza2 = (size2,cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   return  (describePizza betterPizza)

sizeList = [20.0, 15.0]
costList = [18.0, 16.0]

listMain :: [String]
listMain = do
    size1 <- sizeList
    cost1 <- costList 
    size2 <- sizeList
    cost2 <- costList
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)


-- Q31.3
makePizza :: (Monad m) => m Double -> m Double -> m Pizza
makePizza sizeM costM = do
    size <- sizeM
    cost <- costM
    return (size, cost)

monadMain :: (Monad m) => m Pizza -> m Pizza -> m String
monadMain leftM rightM= do
    pizza1 <- leftM
    pizza2 <- rightM
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)