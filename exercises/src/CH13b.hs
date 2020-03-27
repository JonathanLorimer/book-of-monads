{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
module CH13b where

import           Control.Monad.Except
import           Control.Monad.Reader.Class
import           Control.Monad.State
import           Control.Monad.State.Class
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
-- import Data.Functor.Contravariant
-- import Data.Map                   as M
-- import System.IO.Error

data Player = O | X deriving (Eq, Ord)

data Result = AlreadyTaken { takenBy :: Player }
            | NextTurn
            | GameEnded { winner :: Player }


-- 13.1
newtype Position = Position [ Maybe Player ]

freshBoard :: Position
freshBoard = Position
  [ Nothing, Nothing, Nothing
  , Nothing, Nothing, Nothing
  , Nothing, Nothing, Nothing ]

-- 13.2

data Tag = H1 | H2 | H3 | H4 | H5 | H6
         | P
         | Div
         | Span
         | Section
         | Article
          deriving (Show)

data Attr = Attr { key   :: String
                 , value :: String }
                 deriving (Show)

newtype Cmpt props state =
  Cmpt { renderCmpt :: props -> state -> VDom props state}

instance Show (Cmpt props state) where
  showsPrec _ _ = showString "Cmpt"


data HtmlElem =
  HtmlElem { tag        :: Tag
           , attributes :: [Attr] } deriving (Show)

data El props state = Content Text
                    | Component (Cmpt props state)
                    | Elem HtmlElem
                    | Fragment
                    deriving (Show)

data VDom props state = VDomElem (El props state)
                      | VDomNode { root     :: El props state
                                 , children :: [VDom props state] } deriving (Show)


class Monad m => React p s m where
  render   ::      VDom p s -> m (VDom p s)
  setState :: s -> VDom p s -> m (VDom p s)

type Event = String

data AppProps = Props { clickHandler :: Event -> IO ()
                      , inputHandler :: Event -> IO ()}
data AppState = State { user      :: Text
                      , showModal :: Bool }

getEl :: VDom a b -> El a b
getEl (VDomElem e)      = e
getEl VDomNode { root } = root

data ReactError = RenderError Text

newtype ReactApp e r s a = ReactApp { runReactApp :: ExceptT e (ReaderT r (StateT s IO)) a }
  deriving (Functor, Applicative, Monad, MonadReader r, MonadError e, MonadState s)

type ReactAppS = ReactApp ReactError AppProps AppState


renderRoot :: MonadError ReactError m
           => props
           -> state
           -> El props state
           -> m (El props state)
renderRoot p s (Component (Cmpt f)) =
  case f p s of
       VDomNode _ _           -> throwError $ RenderError "the root must be a single element"
       VDomElem (Component c) -> renderRoot p s $ Component c
       VDomElem e             -> pure e
renderRoot _ _ e = pure e

instance React AppProps AppState ReactAppS where

  -- | Render
  render VDomNode { root, children } = do
    pr <- ask
    st <- get
    renderedRoot <- renderRoot pr st root
    renderedChildren <- traverse render children
    pure VDomNode { root = renderedRoot
                  , children = renderedChildren }
  render (VDomElem (Component (Cmpt f))) = do
    pr <- ask
    st <- get
    pure $ f pr st
  render e = pure e

  -- | SetState
  setState s v = do
    put s
    render v







