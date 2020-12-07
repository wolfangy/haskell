lambdaVal1 = (\x -> x) 4
lambdaVal2 = (\x -> x) "hi"
lambdaVal3 = (\x -> x) [1,2,3]

sumSquareOrSquareSumByWhere x y =
    if sumSquare > squareSum
        then sumSquare
        else squareSum
    where
        sumSquare = x^2 + y^2
        squareSum = (x + y)^2

sumSquareOrSquareSumByLambda x y =
    (\sumSquare squareSum -> 
        if sumSquare > squareSum
            then sumSquare
            else squareSum) (x^2 + y^2) ((x + y)^2)

doubleDouble x = (\x -> x * 2) (x * 2)

sumSquareOrSquareSumByLet x y =
    let
        sumSquare = (x^2 + y^2)
        squareSum = (x + y)^2
    in
        if sumSquare > squareSum
            then sumSquare
            else squareSum

overwriteByLet x = let x = 2
    in
        let x = 3
        in
            let x = 4
            in
                x

overwriteByLambda x =
    (\x -> (\x -> (\x -> x) 4) 3) 2


counter x = (\x -> (\x -> x) (x + 1)) (x + 1)