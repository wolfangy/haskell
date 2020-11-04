*ghc* - compiler

*ghci* - interactive interpreter and debugger

*runghc* - run Haskell as scripts

---

**GHCi**
- to change the prompt
`:set prompt "ghci> "`

- to load other modules
`:module + Data.Ratio`

**Operators**
- `/=` for NOT equal

- `not` for logic negative

- left associative is represented as *infixl*
  
- `e` is not defined, to define e: `let e = exp 1`

- a floating-point number as exponent: `(e ** pi) - pi`