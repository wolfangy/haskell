Standard Prelude 

Select first element

> h = head [1..5]

* Remove first element

> t = tail [1..5]

* Select n-th element

> sec = [1..5] !! 2

* Select the first n elements

> first3 = take 3 [1..5]

* Remove the first n elements

> last2 = drop 3 [1..5]

* Calculate the length

> l = length [1..5]

* Calculate the sum of a list

> s = sum [1..5]

* Calculate the product of a list

> p = product [1..5]

* Append two lists

> appended = [1..5] ++ [6..9]

* Reverse a list

> reversed = reverse [1..5]

Haskell can contains cruly parentheses and separate each definition by a semi-colon

> a = b + c
>   where
>       {
>           b = 1;
>           c = 2;
>       };
> d = a * 2

or combined into signle line:

> a' = b + c where { b = 1; c = 2; }; d' = a * 2;


Comments:

> -- Here is the comments

> {-
>    Here is nested comments
>    which can span multiple
>    lines
> -}


> n = a `div` length xs
>   where
>       a = 10
>       xs = [1,2,3,4,5]