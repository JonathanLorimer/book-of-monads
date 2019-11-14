module CH07 where

import Control.Applicative
import Control.Monad.Logic hiding (guard)
import Data.Foldable

-- | Exercise 7.1
data Either' e r = Left' e | Right' r

instance Functor (Either' e) where
  fmap f (Left' e) = Left' e
  fmap f (Right' r) = Right' $ f r

instance Applicative (Either' e) where
  pure = Right'
  (<*>) (Right' f) (Right' a) = Right' $ f a
  (<*>) _ (Left' e) = Left' e
  (<*>) (Left' e) _ = Left' e

instance Monad (Either' e) where
  return = pure
  (>>=) (Left' e)  f = Left' e
  (>>=) (Right' a) f = f a

-- | Exercise 7.2

instance Monoid e => Alternative (Either' e) where
  empty = Left' mempty
  (<|>) (Right' a) _ = Right' a
  (<|>) _         a = a

-- | Exercise 7.3
type Person = String

people :: [Person]
people = ["Alejandro", "Elena", "Quique", "John", "Mary", "Tom"]

pcRels :: [(Person, Person)]
pcRels = [("Alejandro", "Quique")
         ,("Elena", "Quique")
         ,("John", "Mary")
         ,("John", "Tom")
         ,("Mary", "Tim")
         ]

guard :: Alternative m => Bool -> m ()
guard True = pure ()
guard False = empty

siblingRels :: [(Person, Person)]
siblingRels = do
  (p1, c1) <- pcRels
  (p2, c2) <- pcRels
  guard (p1 == p2 && c1 /= c2)
  return (c1, c2)

-- | Exercise 7.4

list :: [a] -> Logic a
list xs = asum (map pure xs)

fairTriples :: [Integer] -> Logic (Integer, Integer, Integer)
fairTriples ns = list ns >>- \x ->
                 list ns >>- \y ->
                 list ns >>- \z ->
                 return (x,y,z)

pyts :: [Integer] -> Logic (Integer, Integer, Integer)
pyts ns = do
  (x,y,z) <- fairTriples ns
  guard (x * x + y * y == z * z)
  return (x,y,z)

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
  throwError = Left
  catchError (Left e) f = f e
  catchError a _ = a

instance MonadError () Maybe where
  throwError _ = Nothing
  catchError Nothing f = f ()
  catchError j _ = j
