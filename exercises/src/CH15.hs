module CH15 where

-- Exercise 15.1
data SList a = SList (forall b. (a -> [a] -> b) -> b -> b)

toSList :: [a] -> SList a
toSList [] = SList $ \_ z -> z
toSList (x:xs) = SList $ \f _ -> f x xs

fromSList :: SList a -> [a]
fromSList (SList f) =  f (:) []

nil :: SList a
nil = SList $ (\_ z -> z)

cons :: a -> SList a -> SList a
cons x xs = SList $ \f' _ -> f' x (fromSList xs)

null :: SList a -> Bool
null (SList f) = f (\_ _ -> False) True

head :: SList a -> a
head (SList f) = f const (error "empty list")

tail :: SList a -> SList a
tail (SList f) = toSList $ f seq (error "empty list")
