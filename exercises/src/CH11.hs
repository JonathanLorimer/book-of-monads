{-# OPTIONS_GHC -fno-warn-orphans #-}
module CH11 where

import Control.Monad.Identity
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State


-- | Exercise 11.1

type Name = String
data Expr = Literal Integer | Var Name | Op Op Expr Expr
data Op   = Add | Subtract | Multiply | Divide

type Assignment = [(Name, Integer)]

eval :: Expr -> MaybeT (State Assignment) Integer
eval (Literal n) = return n
eval (Var v) = withStateMaybe $ lookup v
eval (Op o x y) = do
  u <- eval x
  v <- eval y
  case o of
    Add -> return $ u + v
    Subtract -> return $ u - v
    Multiply -> return $ u * v
    Divide -> if v == 0
                then withStateMaybe $ const Nothing
                else withStateMaybe $ const (Just $ u `div` v)


withStateMaybe :: (b -> Maybe a) -> MaybeT (State b) a
withStateMaybe f = MaybeT $ gets f


-- | Exercise 11.2

-- ExceptT + []
instance {-# Overlaps #-} Monad (ExceptT e []) where
  return = pure
  (ExceptT as) >>= f = ExceptT $ do
    a <- as
    case runExceptT . f <$> a of
      Left e  -> pure $ Left e
      Right x -> x

-- StateT + Either
instance {-# Overlaps #-} Monad (StateT s (Either e)) where
  return = pure
  (StateT sea) >>= f = StateT $ \s -> do
    (a,s') <- sea s
    runStateT (f a) s'

-- ReaderT + IO
instance {-# Overlaps #-} Monad (ReaderT r IO) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

-- ContT + IO
instance {-# Overlaps #-} Monad (ContT r IO) where
  return = pure
  (ContT amrmr) >>= f = ContT $ \bmr -> amrmr $ flip (runContT . f) bmr

-- | Exercise 11.3

toIdentity :: a -> Identity a
toIdentity = pure

fromIdentity :: Identity a -> a
fromIdentity (Identity a) = a

-- | Exercise 11.5

type Parser a = StateT String [] a

{- Original Implementation
satisfies :: (Char -> Bool) -> Parser Char
satisfies p = StateT $ \s ->
  case s of
    "" -> []
    (c:cs) | p c       -> [(c, cs)]
           | otherwise -> []

char :: Char -> Parser Char
char c = satisfies (== c)
-}

char :: Char -> Parser Char
char c = StateT $ \(x:xs) ->
  if c == x
     then [(x,xs)]
     else []

satisfies :: (Char -> Bool) -> Parser Char
satisfies f = StateT go
  where
    go [] = []
    go (c:cs) = do
      guard $ f c
      pure (c,cs)
