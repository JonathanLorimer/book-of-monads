{-# OPTIONS_GHC -fno-warn-orphans #-}
module CH12 where

import Control.Monad.Base
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.ST
import System.IO

-- | Exercise 12.1

liftThroughReaderT :: m a -> ReaderT r m a
liftThroughReaderT x = ReaderT $ const x

class Monad m => MonadReader' r m | m -> r where
    ask'   :: m r
    reader :: (r -> a)
           -> m a
    reader f = f <$> ask'

class (Monad m) => MonadError' e m | m -> e where
    throwError' :: e -> m a
    catchError' :: m a -> (e -> m a) -> m a

class (Monoid w, Monad m) => MonadWriter' w m | m -> w where
    tell' :: w -> m ()


instance MonadReader' r m => MonadReader' r (ReaderT r m) where
  ask' = liftThroughReaderT ask'

instance (MonadError' e m, MonadError' e ((->) r))
       => MonadError' e (ReaderT r m) where
  throwError' e = liftThroughReaderT $ throwError' e
  catchError' m h =
    ReaderT $ catchError' (runReaderT m) (runReaderT . h)


instance MonadWriter' w m => MonadWriter' w (ReaderT r m) where
  tell' x = liftThroughReaderT $ tell' x

-- | Exercise 12.2

class (Monad b, Monad m) => MonadBase' b m | m -> b where
  liftBase' :: b a -> m a

instance MonadBase' IO IO where
  liftBase' = liftIO

instance MonadBase' (ST s) (ST s) where
  liftBase' = liftBase'

instance (Monad (t m), MonadBase b m, MonadTrans t)
       => MonadBase b (t m) where
  liftBase = lift . liftBase

-- | Exercise 12.3

withFile' :: MonadUnliftIO m
          => FilePath -> IOMode -> (Handle -> m r) -> m r
withFile' name mode f = withRunInIO $ \run -> withFile name mode (run . f)


