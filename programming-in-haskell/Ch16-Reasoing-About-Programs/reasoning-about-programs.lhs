> {-# LANGUAGE GADTs #-}

16.1 Equational reasoning

Basic algebraic properties:

Commutative:
    x y := y x

Associative:
    x + (y + z) = (x + y) + z

Distributes:
    x (y + z) = x y + x z
    (x + y) z = x z + y z

the expression:
    x (y + z)
requires two operations, whereas the equivalent expression:
    x y + y z
requires three operations. Even though these two expressions are algebraically 
equal, in terms of efficiency the former si preferable to the later.



