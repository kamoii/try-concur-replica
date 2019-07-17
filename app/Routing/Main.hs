module Main where

import P

--
import qualified Data.Text              as T
import           Text.Read              (readMaybe)
import           Control.Concurrent.STM (retry, check)
import           Control.Concurrent     (threadDelay)
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.ShiftMap
--
import           Concur.Core
import           Concur.Replica

{-|

https://github.com/pkamenarsky/concur/blob/core-types/concur-core/src/Concur/Core/Types.hs

Concur.Replica.DOM
type WidgetConstraints m =
  ( ShiftMap (Widget HTML) m
  , Monad m
  , MonadSafeBlockingIO m
  , MonadUnsafeBlockingIO m
  , MultiAlternative m
  )

-- https://github.com/pkamenarsky/concur/blob/core-types/concur-core/src/Control/MultiAlternative.hs
class MultiAlternative f where
  never :: f a
  orr :: [f a] -> f a

instance {-# OVERLAPPABLE #-} Alternative f => MultiAlternative f where
  never = empty
  orr = foldl (<|>) empty

-- https://github.com/pkamenarsky/concur/blob/core-types/concur-core/src/Control/ShiftMap.hs
-- MonadBase 的な存在なのか？ s 側が base のはず
-- Map系か
-- type (~>) f g = forall x. f x -> g x
class ShiftMap s t where
  shiftMap :: (s ~> s) -> (t ~> t)

instance ShiftMap m m where
  shiftMap = id

instance ShiftMap m (IdentityT m) where
  shiftMap = mapIdentityT

instance ShiftMap m (StateT s m) where
  shiftMap = mapStateT

-- https://github.com/pkamenarsky/concur/blob/core-types/concur-core/src/Concur/Core/Types.hs
class MonadUnsafeBlockingIO m where
  liftUnsafeBlockingIO :: IO a -> m a

instance MonadUnsafeBlockingIO (Widget v) where
  liftUnsafeBlockingIO = io

class MonadSafeBlockingIO m where
  liftSafeBlockingIO :: IO a -> m a

instance MonadSafeBlockingIO (Widget v) where
  liftSafeBlockingIO = effect

-}
class Monad m => MonadRoute r m | m -> r where
  getRoute :: m r
  toRoute :: r -> m a

toRouteM :: MonadRoute r m => m r -> m a
toRouteM = (toRoute=<<)

newtype RouteT r m a = RouteT
  { unRoute :: ExceptT r (ReaderT r m) a
  } deriving (Functor, Applicative, Monad)

-- If you want your custom `loopRouteT` use runRouteT directry.
runRouteT
  :: r
  -> RouteT r m a
  -> m (Either r a)
runRouteT r (RouteT m) = runReaderT (runExceptT m) r

loopRouteT
  :: Monad m
  => r
  -> (forall a. r -> RouteT r m a)
  -> m a
loopRouteT r act = do
  e <- runRouteT r (act r)
  case e of
    Left r' -> loopRouteT r' act
    Right v -> pure v

instance Monad m => MonadRoute r (RouteT r m) where
  getRoute = RouteT $ lift ask
  toRoute  = RouteT . throwError

-- TODO: Is this implementation correct? I'm not sure what `ShiftMap` is.
instance ShiftMap m (RouteT r m) where
  shiftMap f (RouteT a) = RouteT $ mapExceptT (mapReaderT f) a

instance (Monad m, MonadUnsafeBlockingIO m) => MonadUnsafeBlockingIO (RouteT r m) where
  liftUnsafeBlockingIO a = RouteT . lift . lift $ liftUnsafeBlockingIO a

instance (Monad m, MonadSafeBlockingIO m) => MonadSafeBlockingIO (RouteT r m) where
  liftSafeBlockingIO a = RouteT . lift . lift $ liftSafeBlockingIO a

instance (Monad m, Alternative m) => Alternative (RouteT r m) where
  empty = RouteT $ lift $ lift $ empty
  RouteT a <|> RouteT b = RouteT $ do
    r <- ask
    v <- lift $ lift $ runReaderT (runExceptT a) r <|> runReaderT (runExceptT b) r
    case v of
      Left e  -> throwError e
      Right v -> pure v

instance MonadIO m => MonadIO (RouteT r m) where
  liftIO = RouteT . lift . lift . liftIO

-- TODO: Explicit Multie alternative instance for performance


-- `concur-replica`'s `text` has a concrete result type `Widget HTML a`.
-- This code type checks, but I'm not sure if this implementation is correct.
text' :: (Alternative m, ShiftMap (Widget HTML) m) => Text -> m a
text' txt = shiftMap (const $ text txt) empty

data Route = Home | About

-- We currently don't `preventDefault` on client-side, so needs a href="#"
-- to preventing loading pages.
link' :: (MonadRoute r m, _) => r -> Text -> m a
link' r txt = toRouteM $ a [ r <$ onClick, href "#" ] [ text' txt ]

home :: _ => m a
home = do
  div []
    [ h1 [] [ text' "Home" ]
    , p [] [ text' "Welcome! You can play with a counter!" ]
    , manualCounter 0
    , p []
      [ text' "If you want to know about us, visit "
      , link' About "about"
      , text' "."
      ]
    ]

about :: _ => m a
about = do
  div []
    [ h1 [] [ text' "About" ]
    , p []
      [ text' "Nothing to say. Just go "
      , link' Home "home"
      , text' "."
      ]
    ]

manualCounter :: _ => Int -> m a
manualCounter i = do
  div []
    [ text' $ "counter: " <> show i <> " "
    , button [ onClick ] [ text' "inc" ]
    ]
  manualCounter $ i+1

autoCounter :: Int -> Widget HTML a
autoCounter i = do
  liftIO (threadDelay $ 1 * 1000 * 1000) <|> div [] [ text $ show i ]
  autoCounter $ i+1

main :: IO ()
main = do
  runDefault 8080 "mtl-style routing" $ orr
    [ autoCounter 0
    , manualCounter 0
    , loopRouteT Home $ \case
        Home -> home
        About -> about
    ]
