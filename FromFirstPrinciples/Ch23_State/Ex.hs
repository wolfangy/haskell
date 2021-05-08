module Ex where

import Data.Monoid
import Control.Applicative
import Control.Monad

newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap f (Moi g) = Moi $ do
        a <- f . fst . g
        s <- snd . g
        return (a, s)--Moi (\s -> (f (fst(g s)), snd (g s)))

instance Applicative (Moi s) where
    pure a = Moi (\s -> (a, s))

    -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    -- f :: s -> (a -> b, s)
    -- g :: s -> (a, s)
    -- Moi s b :: Moi s -> (b , s)
    Moi f <*> Moi g = Moi (\s -> (,) ((fst $ f s) $ fst $ g s) s)

instance Monad (Moi s) where
    return = pure

    -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    -- Moi f => f :: s -> (a , s)
    -- g :: a -> Moi (s -> (b, s))
    (Moi f) >>= g = Moi $
                    \s -> let (a, s') = f s
                          in runMoi (g a) s'

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

exec :: Moi s a -> s -> s
exec (Moi f) s = snd (f s)

eval :: Moi s a -> s -> a
eval (Moi f) s = fst (f s)


modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
