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


16.2 Reasoning about Haskell

We do not just use properties of built-in operations of the language such as `addition`, 
but also use the equations from which user-defined functions are constructed.

    double :: Int -> Int
    double x = x + x

the `definition` of a function, can also be viewed as `property` that can be used 
when reasoning about this function.

As `logical property`, the equation states that for any integer expression x, 
the expression `x + x` can freely be replaced by `double x`

When reasoning about programs:

* function definitions can be both `applied` from `left-to-right`
* and `unapplied` from `right-to-left`

Some care is required when reasoning about functions that defined using multiple 
equations:

    isZero :: Int -> Bool
    isZero 0 = True
    isZero n = False

`isZero 0 = True` can be viewed as a logical property, can be applied in both direction.

`isZero n = False` can only be replaced by `False` provided that `n != 0`, it is 
only valid to `unapply` the equation `isZero n = False` and replace `False` by 
an expression of the form `isZero n` in the case when `n != 0`.

!!! when a function is defined using multiple equations, the equations cannot be 
viewed as logical properties in isolation from one another, but need to be interrupted 
in light of the order in which patterns are matched within the equations.

!!! it is preferable to define functions in a manner that does not rely on the order 
in which their equations are written.

    isZero 0            = True
    isZero n | n /= 0   = False

Patterns that do not rely on the order in which they are matched are called `non-overlapping`

16.3 Simple example

16.4 Induction on numbers

> data Nat where
>   Zero :: Nat
>   Succ :: Nat -> Nat
>   deriving Show

Suppose we want to prove that some `property`: `p` hold for all (finite) natural 
numbers.

The principle of induction `domino effect`:

`base case`: `p` holds for `Zero`

`inductive case`: `p` is preserved by `Succ`

`induction hypothesis`: if the property `p` holds for any natual number `n`
    then it also holds for `Succ n`

example:

> add :: Nat -> Nat -> Nat
> add Zero      m = m
> add (Succ n)  m = Succ (add n m)

Let's show that the dual property, `add n Zero = n`, abbreviate by `p`:

base case, showing that `p Zero` holds

    add Zero Zero
=   {applying add}
    Zero

inductive case, we must show if `p` holds for any natural number `n`, then `p (Succ n)`
also holds.

induction hypothesis as assumption:
    add n Zero = n
we must show that the equation `add (Succ n) Zero = Succ n` holds

    add (Succ n) Zero
=   {applying add}
    Succ (add n Zero)
=   {induction hypothesis}
    Succ n


Let's show addition of natual number is associative:
    add x (add y z) = add (add x y) z

Base case:
    add Zero (add y z)
=   {applying the outer add}
    add y z
=   {unapplying add}
    add (add Zero y) z

Inductive case:
    add (Succ x) (add y z)
=   {applying the outer add}
    Succ (add x (add y z))
=   {induction hypothesis}
    Succ (add (add x y) z)
=   {unapplying the outer add}
    add (Succ (add x y) z)
=   {unapplying the inner add}
    add (add (Succ x) y) z)