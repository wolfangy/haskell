```haskell
module SimpleJSON (
    JValue(..)
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
) where
...
```
`JValue(..)` means also export the Constructors

without the `..` then only the type is exported without any constructors.

 ** The ability to do this is important: it lets us hide the details of a type from its users, making the type abstract. If we cannot see a type’s value constructors, we cannot pattern match against a value of that type, nor can we construct a new value of that type. **


```haskell
module ExportEverything where
    ...
```
```haskell
module ExportNothing () where
    ...
```

---

`ghc -c SimpleJSON.hs`
- with -c option ghc to generate only object code.
- without ghc will try generate a complete executable
- xxxx.hi is an interface file, in which ghc stores info about the names exported from the module in machine-readable form.