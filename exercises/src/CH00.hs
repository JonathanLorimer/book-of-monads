module CH00 where

data MyTuple a b = MyTuple a b

-- | Exercise 0.2
instance (Eq a, Eq b) => Eq (MyTuple a b) where
  (==) (MyTuple a b) (MyTuple a' b') = a == a' && b == b'

-- | Exercise 0.5
class Container c where
  empty  :: c a
  insert :: a -> c a -> c a

instance Container [] where
  empty = mempty
  insert = (:)

