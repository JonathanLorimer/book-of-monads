module CH05 where

-- | Exercise 5.1

newtype LMaybe a = LMaybe { unLMaybe :: Maybe a } deriving (Eq, Ord, Show)
newtype RMaybe a = RMaybe { unRMaybe :: Maybe a } deriving (Eq, Ord, Show)

instance Semigroup (LMaybe a) where
  (<>) (LMaybe Nothing) lm = lm
  (<>) lm _ = lm

instance Semigroup (LMaybe a) => Monoid (LMaybe a) where
  mempty = LMaybe Nothing
  mappend = (<>)

j1 = LMaybe (Just 1)
j2 = LMaybe (Just 2)
j3 = LMaybe (Just 3)

a = (j1 <> j2) <> j3
b = j1 <> (j2 <> j3)

-- $> a == b

instance Semigroup (RMaybe a) where
  (<>) rm (RMaybe Nothing) = rm
  (<>) _ rm = rm

instance Semigroup (RMaybe a) => Monoid (RMaybe a) where
  mempty = RMaybe Nothing
  mappend = (<>)

g1 = RMaybe (Just 1)
g2 = RMaybe (Just 2)
g3 = RMaybe (Just 3)

c = (g1 <> g2) <> g3
d = g1 <> (g2 <> g3)

-- $> c == d

-- | Exercise 5.2
{-
0^x === 0 && 1^x === 1 === mempty
2^3 `mappend` 2^3
  === 2^3 * 2^3
  === 2^(3 `mappend 3)
  === 2^(3 + 3)
-}
