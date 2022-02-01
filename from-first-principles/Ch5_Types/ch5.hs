module Ch5 where

curry' f a b = f (a, b)

uncurry' f (a, b) = f a b


curry_fst = curry' fst

uncurry_add = uncurry (+)


-- use undefined to only do the definition
--
fn_only_has_deinition :: a -> a -> a -> a
fn_only_has_deinition = undefined

var_only_has_definition :: Char
var_only_has_definition = undefined


kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined
