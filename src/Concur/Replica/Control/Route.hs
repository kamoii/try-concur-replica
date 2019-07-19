{-# LANGUAGE UndecidableInstances #-}

module Concur.Replica.Control.Route where

import P
--
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Base
import           Control.ShiftMap
--
import           Concur.Core
import           Concur.Replica.Extended


class Monad m => MonadRoute r m | m -> r where
  getRoute :: m r
  toRoute :: r -> m a

newtype RouteT r m a = RouteT
  { unRoute :: ExceptT r (ReaderT r m) a
  } deriving (Functor, Applicative, Monad)

toRouteM :: MonadRoute r m => m r -> m a
toRouteM = (toRoute=<<)

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

instance MonadIO m => MonadIO (RouteT r m) where
  liftIO = RouteT . lift . lift . liftIO

instance MonadBase b m => MonadBase b (RouteT r m) where
  liftBase = RouteT . lift . lift . liftBase

instance MonadTrans (RouteT r) where
  lift = RouteT . lift . lift

-- TODO: Is this implementation correct? I'm not sure what `ShiftMap` is.
-- ??? If we stack more than two monad transformer, it could cause a trouble?
--
-- ?? shiftMpa can work like MonadBase ?
-- ?? base monad を決め打ちすれば、
-- ?? Profunctor 的なものを感じる。MnadBase とその逆方向が合さった感じ？
-- MoandBase(Lift) + MonadBaseUnlift?
instance ShiftMap m (RouteT r m) where
  shiftMap f (RouteT a) = RouteT $ mapExceptT (mapReaderT f) a

instance (Monad m, MonadUnsafeBlockingIO m) => MonadUnsafeBlockingIO (RouteT r m) where
  liftUnsafeBlockingIO a = RouteT . lift . lift $ liftUnsafeBlockingIO a

instance (Monad m, MonadSafeBlockingIO m) => MonadSafeBlockingIO (RouteT r m) where
  liftSafeBlockingIO a = RouteT . lift . lift $ liftSafeBlockingIO a

instance (Monad m, Alternative m) => Alternative (RouteT r m) where
  empty = RouteT . lift . lift $ empty
  RouteT a <|> RouteT b = RouteT $ do
    r <- ask
    v <- lift . lift $ runReaderT (runExceptT a) r <|> runReaderT (runExceptT b) r
    case v of
      Left e  -> throwError e
      Right v -> pure v

-- TODO: Explicit Multie alternative instance for performance

-- We currently don't `preventDefault` on client-side, so needs a href="#"
-- to preventing loading pages.
link'
  :: (MonadRoute r m, MonadBase (Widget HTML) m, WidgetConstraints m)
  => r
  -> Text
  -> m a
link' r txt = toRouteM $ a [ r <$ onClick, href "#" ] [ text' txt ]
