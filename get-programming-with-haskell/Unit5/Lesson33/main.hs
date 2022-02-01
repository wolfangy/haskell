import Control.Monad
import Control.Applicative 

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

_select :: Monad m => (a -> b) -> m a-> m b
_select prop vals = do
    val <- vals
    return (prop val)

-- where signature:
-- (a -> Bool) -> [a] -> [b]

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
    val <- vals
    guard (test val)
    return val

-- join signature::
-- Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join left right propL propR = do
    l <- left
    r <- right
    let lp = propL l
    let rp = propR r
    guard (lp == rp)
    return (l, r)

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                    | HINQ_ (m a -> m b) (m a)

_hinq selectQuery joinQuery whereQuery = (\joinData -> 
    (\whereResult -> selectQuery whereResult) (whereQuery joinData)) joinQuery 

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((== "English") . courseTitle . snd))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
                (_join teachers courses teacherId teacher)
                (_where ((== "English") . courseTitle .  snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

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

teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName) finalResult (_where (\_ -> True))

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)


possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
                    (_join possibleTeacher possibleCourse teacherId teacher)
                    (_where ((== "French") . courseTitle . snd))

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ (_select (teacherName . fst))
                    (_join possibleTeacher missingCourse teacherId teacher)
                    (_where ((== "French") . courseTitle . snd))

data Enrollment = Enrollment {
    student :: Int
    , course :: Int
} deriving Show

enrollments :: [Enrollment]
enrollments = [
    (Enrollment 1 101) 
    ,(Enrollment 2 101)
    ,(Enrollment 2 201)
    ,(Enrollment 3 101)
    ,(Enrollment 4 201)
    ,(Enrollment 4 101)
    ,(Enrollment 5 101)
    ,(Enrollment 6 201) ]


studentEnrollmentsQuery = HINQ_ (_select (\ (st, en) -> (studentName st, course en)))
                            (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQuery


englishStudentsQuery  = HINQ (_select (fst . fst))
                            (_join studentEnrollments courses snd courseId)
                            (_where ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQuery