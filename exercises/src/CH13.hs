{-# LANGUAGE GADTs #-}
module CH13 where

import Data.Map as M
import Control.Monad.Trans.Reader
import Control.Monad.State
import Data.Functor.Contravariant
import System.IO.Error

data Player = O | X deriving (Eq, Ord)


-- | Exercise 13.1
type Columns a = [a]
type Rows a = [a]
newtype Position = Position { unPosition :: Rows (Columns Player) }

-- | Exercise 13.2
-- React Component

-- | Exercises 13.3
newtype Component p = Component { unComponent :: p -> HTML}
type Inert = Component ()
type HTML = String
-- render :: Component a -> a -> HTML
-- nest :: Component a -> Component b -> a -> Component (b, HTML)

pureRender :: a -> Component a -> HTML
pureRender a (Component ca) = ca a

class FrontEnd m where
  render :: m (Component a) -> a -> m HTML
  nest :: m (Component a) -> m (Component (HTML, b)) -> a -> m (Component b)
  coupled :: (b -> a) -> m (Component a) -> m (Component (HTML, b)) -> (b -> m (Component b))
  effComp :: IO a -> Component a -> m (Component ())

instance Contravariant Component where
  contramap f fb = Component $ unComponent fb . f

type Env = M.Map String String

instance FrontEnd (ReaderT Env IO) where
  render mca a = ($ a) . unComponent <$> mca
  nest ca cb a = fmap Component $ curry . unComponent <$> cb <*> render ca a
  coupled f ca cb =
     \b -> fmap Component
        $  fmap (curry . unComponent) cb
       <*> fmap (pureRender b . contramap f) ca
  effComp ia (Component ca) = ReaderT $ \_ -> do
    a <- ia
    return $ Component ((\_ -> ca a) :: () -> HTML)

-- | Exercise 13.4

type FSError = IOError

class FS m where
  writeFile :: FilePath -> String -> m (Either FSError ())
  readFile :: FilePath -> m (Either FSError String)

type MockFileSystem = M.Map FilePath String


instance FS (State MockFileSystem) where
  writeFile fp f = do
    st   <- get
    updated <- put $ M.insert fp f st
    return $ Right updated
  readFile fp = do
    st <- get
    return $ case M.lookup fp st of
      Just s  -> Right s
      Nothing -> Left $ mkIOError userErrorType "error" Nothing (Just fp)

-- | Exercise 13.5

from :: (() -> a) -> a
from = ($ ())

to :: a -> (() -> a)
to a _ = a

-- | Exercise 13.6
data Result = AlreadyTaken { by' :: Player }
            | NextTurn
            | GameEnded { winner :: Player }


data TicTacToe a = Info Position (Maybe Player -> TicTacToe a)
                 | Take Position (Result       -> TicTacToe a)
                 | Done a

instance Functor TicTacToe where
  fmap f (Done a) = Done $ f a
  fmap f (Info p k) = Info p (fmap f . k)
  fmap f (Take p k) = Take p (fmap f . k)

{- My Shitty Implementation
instance Applicative TicTacToe where
  pure = Done
  (Done f) <*> v = fmap f v
  it <*> (Done a) = fmap ($ a) it
  (Info _ k) <*> (Info p' k') = Info p' $ \mp -> k mp <*> k' mp
  (Take _ k) <*> (Take p' k') = Take p' $ \r -> k r <*> k' r
  (Info _ k) <*> (Take p' k') = Take p' $ \r -> k Nothing <*> k' r
  (Take _ k) <*> (Info p' k') = Take p' $ \r -> k r <*> k' Nothing
-}

instance Applicative TicTacToe where
  pure = Done
  (<*>) (Done f)   v = f <$> v
  (<*>) (Info p r) v = Info p $ (<*> v) . r
  (<*>) (Take p r) v = Take p $ (<*> v) . r

-- | Exercise 13.8

data FS' a = WriteFile FilePath String (Either FSError ()     -> FS' a)
           | ReadFile  FilePath        (Either FSError String -> FS' a)
           | FSDone                    a

interpret :: FS' a -> State MockFileSystem a
interpret (FSDone a) = return a
interpret (WriteFile fp s k) = do
    st   <- get
    _ <- put $ M.insert fp s st
    interpret (k $ Right ())
interpret (ReadFile fp k) = do
    st <- get
    interpret $ k $ case M.lookup fp st of
      Just s  -> Right s
      Nothing -> Left $ mkIOError userErrorType "error" Nothing (Just fp)

 -- | Exercise 13.9
data TicTacToeF r = InfoF Position (Maybe Player -> r)
                  | TakeF Position (Result       -> r)


instance Functor TicTacToeF where
  fmap f (InfoF p k) = InfoF p $ f . k
  fmap f (TakeF p k) = TakeF p $ f . k

-- | Exercies 13.10

data Free f a = Free (f (Free f a))
              | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free x) = Free (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> v = f <$> v
  Free f <*> v = Free $ fmap (<*> v) f

instance Functor f => Monad (Free f) where
  return = Pure
  Pure x >>= f = f x
  Free x >>= f = Free (fmap (>>= f) x)

{-
fmap f (InfoF p k) = InfoF p $ f . k
=== -- Introduce Free constructor
fmap f (Free (Info p k)) = Free $ (Info p $ f . k)
=== -- Convert composition to fmap by definition of fmap
fmap f (Free (Info p k)) = Free $ (Info p $ fmap f k)
=== -- Move fmap outside of Info constructor via another fmap
fmap f (Free (Info p k)) = Free $ fmap (fmap f) (Info p k)
=== -- Abstract Info into a variable
fmap f (Free x) = Free $ fmap (fmap f) x
-}

-- | Exercise 13.11

data FSF r = WriteFileF FilePath String (Either FSError ()     -> r)
           | ReadFileF  FilePath        (Either FSError String -> r)

instance Functor FSF where
  fmap f (WriteFileF fp s k) = WriteFileF fp s $ f . k
  fmap f (ReadFileF  fp   k) = ReadFileF  fp   $ f . k

foldFree :: Monad m => (forall r . f r -> m r) -> Free f a -> m a
foldFree _   (Pure x) = return x
foldFree int (Free x) = do x' <- int x
                           foldFree int x'


-- | Exercise 13.12

data TicTacToeOP a where
  InfoOP :: Position -> TicTacToeOP (Maybe Player)
  TakeOP :: Position -> TicTacToeOP Result
  TTDone :: a        -> TicTacToeOP a
  TTBind :: TicTacToeOP a -> (a -> TicTacToeOP b) -> TicTacToeOP b

instance Functor TicTacToeOP where
  fmap f t = t `TTBind` (TTDone . f)

-- | Exercise 13.13

data FSOP a where
  WriteFileOP :: FilePath -> String -> FSOP (Either FSError ())
  ReadFileOP  :: FilePath           -> FSOP (Either FSError String)
  FSDoneOP    :: a                  -> FSOP a
  FSBindOP    :: FSOP a -> (a -> FSOP b) -> FSOP b

  {-
instance Monad FSOP where
  return = FSDoneOP
  (>>=) = FSBindOP
  -}

-- | Exercise 13.14

data Program instr a where
  PDone  :: a -> Program instr a
  PBind  :: Program instr b -> (b -> Program instr a) -> Program instr a
  PInstr :: instr a -> Program instr a


instance Functor (Program instr) where
  fmap f p = p `PBind` (PDone . f)

instance Applicative (Program instr) where
  pure = PDone
  (<*>) (PDone f) p = f <$> p
  (<*>) (PBind m k) p = PBind m $ (<*> p) . k
  (<*>) pinstr p = pinstr <*> p

instance Monad (Program isntr) where
  return = PDone
  (>>=)  = PBind
