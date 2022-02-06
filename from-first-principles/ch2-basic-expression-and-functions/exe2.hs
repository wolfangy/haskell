module Exe2 where
z = 7
x = y ^ 2
waxOn = x * 5
y = z + 8

waxOn' = x * 5
    where 
        x = y ^ 2
        y = z + 8
        z = 7

triple x = x * 3

waxOff x = triple x
