import Control.Monad

data Name = Name {
    firstName ::String
    , lastName :: String
 }

instance Show Name where
     show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman
    | Sophmore
    | Junior
    | Senior
    deriving (Eq,Ord,Enum,Show)

data Student = Student {
    studentId :: Int
    , gradeLevel :: GradeLevel
    , studentName :: Name
} deriving Show

students :: [Student]
students = [
    (Student 1 Senior (Name "Audre" "Lorde"))
    , (Student 2 Junior (Name "Leslie" "Silko"))
    , (Student 3 Freshman (Name "Judith" "Butler"))
    , (Student 4 Senior (Name "Guy" "Debord"))
    , (Student 5 Sophmore (Name "Jean" "Baudrillard"))
    , (Student 6 Junior (Name "Julia" "Kristeva"))
    ]

data Teacher = Teacher {
    teacherId :: Int
    , teacherName :: Name } deriving Show

teachers :: [Teacher]
teachers = [
    Teacher 100 (Name "Simone" "De Beauvior")           
    ,Teacher 200 (Name "Susan" "Sontag") ]

data Course = Course { 
    courseId :: Int , 
    courseTitle :: String , 
    teacher :: Int } deriving Show

courses :: [Course]
courses = [ Course 101 "French" 100, Course 201 "English" 200 ]

-- select signature:
-- (a -> b) -> [a] -> [b]

_select :: (a -> b) -> [a] -> [b]
_select prop vals = do
    val <- vals
    return (prop val)

-- where signature:
-- (a -> Bool) -> [a] -> [b]

_where :: (a -> Bool) -> [a] -> [a]
_where test vals = do
    val <- vals
    guard (test val)
    return val

-- join signature::
-- Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join left right propL propR = do
    l <- left
    r <- right
    let lp = propL l
    let rp = propR r
    guard (lp == rp)
    return (l, r)

_hinq selectQuery joinQuery whereQuery = (\joinData -> 
    (\whereResult -> selectQuery whereResult) (whereQuery joinData)) joinQuery 

finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((== "English") . courseTitle . snd))

fizzBuzz' :: Int -> String
fizzBuzz' num
    | num `mod` 3 == 0 && num `mod` 5 == 0  = "FizzBuzz"
    | num `mod` 3 == 0                      = "Fizz"
    | num `mod` 5 == 0                      = "Buzz"
    | otherwise                             = show num

printFizzBuzzArray :: Int -> IO ()
printFizzBuzzArray n = mapM_ print (fizzBuzz' <$> [1..n])

printFizzBuzzArray' :: Int -> IO ()
printFizzBuzzArray' n = mapM_ print [fb | val <- [1..n], let fb = if val `mod` 15 == 0 then "FizzBuzz" else if val `mod` 3 == 0 then "Fizz" else if val `mod` 5 == 0 then "Buzz" else show val]

printArray :: [String] -> IO ()
printArray strs = do
    str <- return strs
    print str