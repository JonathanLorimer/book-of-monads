module CH04 where

-- | Exercise 4.1
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence $ zipWith f xs ys

replicateM :: Monad m => Int -> m a -> m [a]
replicateM i = sequence . replicate i

-- filterM doesn't work because the result of filtering a -> m Bool is not [m Bool] it is a [a]

-- |
