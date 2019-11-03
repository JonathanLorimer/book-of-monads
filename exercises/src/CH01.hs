module CH01 where

type State s a = s -> (a, s)

-- | Exercise 1.1
next :: State s a -> (a -> State s b) -> State s b
next f g i = let (r, i') = f i in g r i'

pure :: State s a
pure a s = (s, a)

-- | Exercise 1.3
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- | Exercise 1.4
then_ :: Maybe (Maybe b) -> Maybe b
then_ Nothing = Nothing
then_ Just v  = v
