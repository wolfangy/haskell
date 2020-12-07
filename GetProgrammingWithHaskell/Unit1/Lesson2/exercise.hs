inc x = x + 1
double x = x * 2
square x = x * x

q2_3_func n = 
    if mod n 2 == 0
        then evenVal
        else oddVal
    where
        evenVal = n -2
        oddVal = 3 * n + 1
