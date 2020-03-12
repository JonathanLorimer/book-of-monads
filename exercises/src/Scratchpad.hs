{-# LANGUAGE FunctionalDependencies #-}
module Scratchpad where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad.ST

-- 11.1
type Name = String
type Assignment = [(Name, Integer)]
data Expr = Literal Integer
          | Var Name
          | Op Op Expr Expr
data Op = Add | Subtract | Multiply | Divide

evalFail :: Applicative f => MaybeT f a
evalFail = MaybeT $ pure Nothing

eval :: Expr -> MaybeT (State Assignment) Integer
eval (Literal n) = return n
eval (Var v)     = do
  s <- MaybeT $ Just <$> get
  case lookup v s of
    Nothing -> evalFail
    Just v' -> return v'
eval (Op o x y)  = do
  u <- eval x
  v <- eval y
  case o of
    Add       -> return (u + v)
    Subtract  -> return (u - v)
    Multiply  -> return (u * v)
    Divide    -> if v == 0 then evalFail
                           else return (u `div` v)


-- 12.2

class (Monad b, Monad m) => MonadBase b m | m -> b where
  liftBase :: b a -> m a

instance MonadBase IO IO where
  liftBase = liftBase

instance MonadBase (ST s) (ST s) where
  liftBase = liftBase

instance (  Monad (t m)
         ,  MonadBase b m
         ,  MonadTrans t )
         => MonadBase b (t m) where
  liftBase = lift . liftBase


