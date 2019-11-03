module CH01 where

type State s a = s -> (a, s)

-- | Exercise 1.1
next :: State s a -> (a -> State s b) -> State s b
next f g i = let (r, i') = f i in g r i'

pure :: a -> State s a
pure a = \s -> (a, s)

-- | Exercise 1.3
map_ :: (a -> b) -> [a] -> [b]
map_ _ [] = []
map_ f (x:xs) = f x : map_ f xs

-- | Exercise 1.4
then_ :: Maybe (Maybe b) -> Maybe b
then_ Nothing   = Nothing
then_ (Just v)  = v
