module CH06 where

import Data.Functor.Contravariant

-- | Exercise 6.1
newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f (State a) = State $ \s ->
    let (a', s') = a s
     in (f a', s')

instance Applicative (State s) where
  pure a = State $ \s -> (a,s)
  (<*>) (State f) (State a) = State $ \s ->
    let (f', s') = f s
        (a', s'') = a s'
      in (f' a', s'')

instance Monad (State s) where
  return = pure
  (>>=) (State a) amb = State $ \s ->
    let (a', s') = a s
     in ($ s') . runState . amb $ a'

-- | Exercise 6.2
get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put $ f s

-- | Reader Experiments

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader a) = Reader $ f . a

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a
  (<*>) (Reader f) (Reader a) = Reader $ \r -> f r $ a r

instance Monad (Reader r) where
  return = pure
  (>>=) (Reader ra) arb = Reader $ \r ->
    ($ r) . runReader . arb $ ra r


-- | Exercise 6.3
newtype Returns r a = R { unReturn :: a -> r }

instance Contravariant (Returns r) where
  contramap f (R ar) = R $ ar . f

