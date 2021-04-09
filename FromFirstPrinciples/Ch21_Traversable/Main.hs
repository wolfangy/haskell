module Main where

import Data.Monoid
import Data.Maybe
import Data.Functor.Identity
import Data.Functor.Constant

-- Traversable allows you totransform elements inside the strcture like a functor,
-- producing applicative effects along the way, and lift those potentially multiple
-- instances of applicative strcture outside of the traversable strcture.
--
-- class (Functor t, Foldable t) => Traversable t where 
--
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse f = sequenceA . fmap f
--
-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- sequenceA = traverse id
--
arrJust = fmap Just [1..3]
justArr = sequenceA $ fmap Just [1..3]
nothing = sequenceA [Just 1, Just 2, Nothing]

arrByCat1 =catMaybes [Just 1, Just 2, Just 3]
arrByCat2 = catMaybes [Just 1, Just 2, Nothing]


-- fmap     :: (a ->   b) -> f a -> f  b
-- (=<<)    :: (a -> m b) -> m a -> m  b
-- traverse :: (a -> f b) -> t a -> f (t b)
-- mapM     :: (a -> m b) -> [a] -> m [b]

justArr' = traverse Just [1,2,3]


data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
    a <- fetchFn query
    case sequence (map decodeFn a) of
        (Left err) -> return $ Left err
        (Right res) -> do
            a <- makeIoOnlyObj res
            return $ Right a

pipelineFn1 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn1 query = do
    a <- fetchFn query
    traverse makeIoOnlyObj (mapM decodeFn a)

pipelineFn2 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn2 = (traverse makeIoOnlyObj . mapM decodeFn =<<) . fetchFn

pipelineFn3 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn3 = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

-- Traversable is stronger than Functor and Foldable
-- recover the Functor and Foldable instances for a type from the Traversable
--
edgeMap :: Traversable f  => (a -> b) -> f a -> f b
edgeMap f t = runIdentity $ traverse (Identity . f) t

foldMap :: (Traversable f, Monoid b) => (a -> b) -> f a -> b
foldMap f t = getConstant $ traverse (Constant . f) t

--
main :: IO ()
main = undefined
