module CH09 where

import Control.Monad.Cont

-- | Practice

newtype Bracket' r a = Bracket'
  { unBracket' :: IO r             -- acquisition
               -> (r -> IO a)      -- use
               -> (IO r -> IO ())  -- release
               -> IO a }           -- result

newtype MyCont r a = MyCont { unCont :: (a -> r) -> r }

instance Functor (MyCont r) where
  fmap f (MyCont arr) = MyCont $ \br -> arr $ br . f

instance Applicative (MyCont r) where
  pure a = MyCont $ \ar -> ar a
  (MyCont f) <*> arr =
    MyCont $ \br -> f (\ab -> (unCont $ fmap ab arr) br )

instance Monad (MyCont r) where
  return = pure
  (MyCont arr) >>= abrr =
    MyCont $ \br -> arr (\a -> unCont (abrr a) br)


-- | Exercise 9.1
toCont :: a -> (forall r. Cont r a)
toCont a = cont (\ar -> ar a)

fromCont :: (forall r. Cont r a) -> a
fromCont cf = runCont cf id


