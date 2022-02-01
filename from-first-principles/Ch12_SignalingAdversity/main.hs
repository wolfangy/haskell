module Ch12 where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty 
                    | AgeTooLow
                    deriving Eq

toString :: PersonInvalid -> String
toString NameEmpty = "Person_Name_Cannot_Be_Empty"
toString AgeTooLow = "Person_Age_Not_In_Range"

instance Show PersonInvalid where show = toString


ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age =
    case age >= 0 && age <= 160 of
        True -> Right age
        False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name =
    case name /= "" of
        True -> Right name
        False -> Left [NameEmpty]

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age 
    | name /= "" && age >= 0 && age < 160 = Right $ Person name age
    | name == "" = Left NameEmpty
    | otherwise = Left AgeTooLow

type ValidatePerson a = Either [PersonInvalid] a

mkPerson' :: Name -> Age -> ValidatePerson Person
mkPerson' name age = mkPersonImpl (nameOkay name) (ageOkay age)
    where
        mkPersonImpl :: ValidatePerson Name -> 
                        ValidatePerson Age -> 
                        ValidatePerson Person
        mkPersonImpl (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
        mkPersonImpl (Left badName) (Left badAge) = Left (badName ++ badAge)
        mkPersonImpl (Left badName) _ = Left badName
        mkPersonImpl _ (Left badAge) = Left badAge
