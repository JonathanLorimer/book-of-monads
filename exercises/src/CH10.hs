{-# LANGUAGE TupleSections #-}

module CH10 where

-- | Notes

newtype (f :.: g) a = Compose (f(g a))

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Compose fga) = Compose $ fmap (fmap f) fga

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure a = Compose $ pure $ pure a
  (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

instance (Foldable (f :.: g), Traversable f, Traversable g)
         => Traversable (f :.: g) where
  traverse f (Compose g) = Compose <$> traverse (traverse f) g

-- | Exercise 10.1

class (Monad m, Monad n) => Swappable m n where
  swap :: m (n a) -> n (m a)

instance (Monad m) => Swappable Maybe m where
  swap Nothing = pure Nothing
  swap (Just x) = fmap Just x


newtype Writer w a = Writer { unWriter :: (a, w) }
  deriving (Eq, Ord, Show)

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  (Writer (f, w)) <*> (Writer (a, w')) = Writer (f a, w <> w')

instance Monoid w => Monad (Writer w) where
  return = pure
  (Writer (a, w)) >>= f = Writer $ fmap (w <>) . unWriter . f $ a

instance (Monad m, Monoid w) => Swappable (Writer w) m where
  swap (Writer (a, w)) = fmap (Writer . (,w)) a

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader a) = Reader $ f . a

instance Applicative (Reader r) where
  pure a = Reader $ const a
  (<*>) (Reader f) (Reader a) = Reader $ \r -> f r $ a r

instance Monad (Reader r) where
  return = pure
  (>>=) (Reader ra) arb = Reader $ \r ->
    ($ r) . runReader . arb $ ra r

instance Swappable (Reader r) (Reader s) where
  swap (Reader f) = Reader $ \s -> Reader $ \r -> runReader (f r) s

-- | Exercise 10.2

newtype Listed m a = Listed { unListed :: [m a] }

instance (Monad m) => Functor (Listed m) where
  fmap f (Listed xs) = Listed $ fmap (fmap f) xs

instance (Monad m) => Applicative (Listed m) where
  pure a = Listed [pure a]
  (Listed fs) <*> (Listed as) = Listed $ fmap (<*>) fs <*> as
