module CH03 where

-- | Exercise 3.1
ap :: Monad m => m (b -> c) -> m b -> m c
ap mf mb = do
  f <- mf
  b <- mb
  return $ f b

ap' :: Monad m => m (b -> c) -> m b -> m c
ap' mf mb = mf >>= (<$> mb)

-- | Exercise 3.2
fmap_ :: Applicative f => (a -> b) -> f a -> f b
fmap_ f a = pure f <*> a

-- | Exercise 3.3
newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ fmap f xs

-- | Exercise 3.4
nestTriple :: (a, b, c) -> (a, (b, c))
nestTriple (a, b, c) = (a, (b, c))

nestQuad :: (a, b, c, d) -> (a, (b, (c, d)))
nestQuad (a, b, c, d) = (a, (b, (c, d)))

-- | Exercise 3.5

unit :: Applicative f => f ()
unit = pure ()

(**) :: Applicative f => f a -> f b -> f (a, b)
(**) a b = (,) <$> a <*> b


