names = [("Ian", "Curtis"),
         ("Bernard","Sumner"),
         ("Peter", "Hook"),
         ("Adam", "Summer"),
         ("Stephen","Morris")];

compareFn part1 part2 =
    if part1 > part2
        then GT
        else if part1 < part2
            then LT
            else EQ

compareNames name1 name2 =
    if compareFn lastName1 lastName2 == EQ
        then compareFn firstName1 firstName2
        else compareFn lastName1 lastName2
    where
        firstName1 = fst name1
        lastName1 = snd name1
        firstName2 = fst name2
        lastName2 = snd name2


sfOffice name = 
    if lastName < "L"
        then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
        else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
        
    where
        lastName = snd name
        nameText = (fst name) ++ " " ++ lastName

nyOffice name =
    nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name =
    nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = snd name

getLocationFunction location =
    case location 
    of 
        "ny" -> nyOffice
        "sf" -> sfOffice
        "reno" -> renoOffice
        _ -> (\name -> (fst name) ++ " " ++ (snd name))