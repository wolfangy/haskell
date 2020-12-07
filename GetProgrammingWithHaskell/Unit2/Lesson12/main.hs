type FirstName = String
type Lastname = String

type Age = Int
type Height = Int

patientInfo :: FirstName -> Lastname -> Age -> Height -> String
patientInfo fname lname age height = 
    name ++ " " ++ ageHeight
    where
        name = lname ++ ", " ++ fname
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type PatientName = (FirstName, Lastname)

patientInfoV2 :: PatientName -> Age -> Height -> String
patientInfoV2 (fname, lname) age height =
    name ++ " " ++ ageHeight
    where
        name = lname ++ ", " ++ fname
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

data RhType = Pos | Neg
data ABOType =  A | B | AB | O
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos


patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO O = "O"
showABO AB = "AB"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type MiddleName = String
data Name = Name FirstName Lastname
            | NameWithMiddle FirstName MiddleName Lastname

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

type Weight = Int

data Patient = Patient Name Sex Age Height Weight BloodType

janeElizabethSmith = Patient (NameWithMiddle "Jame" "Elizabeth" "Smith") Female 28 62 140 (BloodType AB Pos)

getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getSex :: Patient -> Sex
getSex (Patient _ s _ _ _ _) = s

getAge :: Patient -> Age
getAge (Patient _ _ a _ _ _) = a

getHeight :: Patient -> Height
getHeight (Patient _ _ _ h _ _) = h

getWeight :: Patient -> Weight
getWeight (Patient _ _ _ _ w _) = w

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt

data PatientV2 = PatientV2 {
    name :: Name
    , sex :: Sex
    , age :: Age
    , height :: Height
    , weight :: Weight
    , bloodType :: BloodType
}

jackieSmith :: PatientV2
jackieSmith = PatientV2 {name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }
jackieHeight = height jackieSmith
jackieBloodType = showBloodType (bloodType jackieSmith)
jakieName = showName (name jackieSmith)

jackieSmithUpdated = jackieSmith { age = 44 }


convertPatientType :: Patient -> PatientV2
convertPatientType p =
    PatientV2{
        name = getName p
        , age = getAge p
        , sex = getSex p
        , height = getHeight p
        , weight = getWeight p
        , bloodType = getBloodType p
    }

-- Q12.1 
canDonateBetween :: PatientV2 -> PatientV2 -> Bool
canDonateBetween p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

-- Q12.2
patientSummary :: PatientV2 -> String
patientSummary p =
    showStar ++ showRT 
    ++ (showPatientName p) ++ showRT
    ++ (showPatientSex p) ++ showRT
    ++ (showPatientAge p) ++ showRT
    ++ (showPatientHeight p) ++ showRT
    ++ (showPatientWeight p) ++ showRT
    ++ (showPatientBloodType p) ++ showRT
    ++ showStar ++ showRT
    where
        showStar = "**************"
        showRT = "\r\n"
        showPatientName p = "Patient Name: " ++ showName (name p)
        showPatientSex p = "Sex: " ++ showSex (sex p)
        showPatientAge p = "Age: " ++ show (age p)
        showPatientHeight p = "Height: " ++ show (height p)
        showPatientWeight p = "Weight: " ++ show (weight p)
        showPatientBloodType p = "Blood Type: " ++ showBloodType (bloodType p)
-- use putStr in GHCi