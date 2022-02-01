data SixSidedDie =  S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)

instance Show SixSidedDie where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"

instance Eq SixSidedDie where
    (==) S6 S6 = True
    (==) S5 S5 = True
    (==) S4 S4 = True
    (==) S3 S3 = True
    (==) S2 S2 = True
    (==) S1 S1 = True
    (==) _ _ = False

instance Ord SixSidedDie where
    compare S6 S6 = EQ
    compare S6 _ = GT
    compare _ S6 = LT
    compare S5 S5 = EQ
    compare S5 _ = GT
    compare _ S5 = LT
    compare S4 S4 = EQ
    compare S4 _ = GT
    compare _ S4 = LT
    compare S3 S3 = EQ
    compare S3 _ = GT
    compare _ S3 = LT
    compare S2 S2 = EQ
    compare S2 S1 = GT
    compare S1 S2 = LT
    compare S1 S1 = EQ

data SixSidedDieV2 =   Side1 | Side2 | Side3 | Side4 | Side5 | Side6 deriving (Eq, Ord, Enum)
instance Show SixSidedDieV2 where
    show Side1 = "1"
    show Side2 = "2"
    show Side3 = "3"
    show Side4 = "4"
    show Side5 = "5"
    show Side6 = "6"


-- instance Enum SixSidedDie where
--    toEnum 0 = S1
--    toEnum 1 = S2
--    toEnum 2 = S3
--    toEnum 3 = S4
--    toEnum 4 = S5
--    toEnum 5 = S6
--    toEnum _ = error "No such value"

--    fromEnum S1 = 0
--    fromEnum S2 = 1
--    fromEnum S3 = 2
--    fromEnum S4 = 3
--    fromEnum S5 = 4
--    fromEnum S6 = 5


data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
   compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

names :: [Name]
names = [Name ("Emil","Cioran"), Name ("Eugene","Thacker") , Name ("Friedrich","Nietzsche")]

newtype NameV2 = NameV2 (String, String) deriving (Show, Eq)
namesV2 :: [NameV2]
namesV2 = [NameV2 ("Emil","Cioran"), NameV2 ("Eugene","Thacker") , NameV2 ("Friedrich","Nietzsche")]



-- Q14.1
data SixSidedDieV3 = S1_V3 | S2_V3 | S3_V3 | S4_V3 | S5_V3 | S6_V3 deriving (Enum)
instance Show SixSidedDieV3 where
    show S1_V3 = "3-I"
    show S2_V3 = "3-II"
    show S3_V3 = "3-III"
    show S4_V3 = "3-IV"
    show S5_V3 = "3-V"
    show S6_V3 = "3-VI"

instance Eq SixSidedDieV3 where
    (==) d1 d2 = (fromEnum d1) == (fromEnum d2)

instance Ord SixSidedDieV3 where
    compare d1 d2 = compare (fromEnum d1) (fromEnum d2)

-- Q14.2
class Die (Eq a, Enum a) where
    roll :: Int -> a

data FiveSidedDie = FS1 | FS2 | FS3 | FS4 | FS5 deriving (Show, Eq, Ord, Enum)
instance Die FiveSidedDie where
    roll n = toEnum (n 'mod' 5)