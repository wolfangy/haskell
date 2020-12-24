module Employee where

data Employee = Coder
               | Manager
               | Veep
               | CEO
               deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) 
                -> Employee -> Employee -> IO ()
employeeRank cmp e e' =  case cmp e e' of
                        GT -> reportBoss e e'
                        EQ -> putStrLn "Neither emploee\
                                        \ is the boss"
                        LT -> (flip reportBoss) e e'

normalCompare :: Employee -> Employee -> Ordering
normalCompare = compare

coderCompare :: Employee -> Employee -> Ordering
coderCompare Coder Coder = EQ
coderCompare Coder _ = GT
coderCompare _ Coder = LT
coderCompare e e' = compare e e'
