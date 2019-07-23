{-# LANGUAGE TypeApplications #-}
module Concur.Replica.Control.Exception
  ( ReleaseStack
  , acquire
  , release
  , pbracket
  ) where

import P
import Control.Exception (SomeException, mask_, try)

{-|

-}


newtype ReleaseStack = ReleaseStack (IORef [IO ()])

acquire :: IO ReleaseStack
acquire = ReleaseStack <$> newIORef []

-- 例外を投げる意味はないため try で握り潰す
-- mask された状態で実行される想定のため別途maskは不要
release :: ReleaseStack -> IO ()
release (ReleaseStack rs) = do
  stk <- readIORef rs
  traverse_ (\rel -> try @SomeException rel) stk

-- | pseudo-bracket
--
-- Acquire and Release function will be invoked inside mask.
-- Monad m は通常の例外補足ができないという前提。でないと補足されて
-- 処理されちゃうと破綻する。一度でも例外が投げられると根本まで上って
-- `release`関数に溜ったリソース解放処理を順次実行させる。
pbracket
  :: MonadIO m
  => ReleaseStack
  -> IO r
  -> (r -> IO ())
  -> (r -> m a)
  -> m a
pbracket (ReleaseStack rs) acq rel_ m = do
  (res, rel, oldStk) <- liftIO $ mask_ $ do
    res <- acq                  -- (*)
    let rel = mask_ $ rel_ res
    oldStk <- atomicModifyIORef rs $ \stk -> (rel:stk, stk)
    pure (res, rel, oldStk)
  a <- m res                    -- (*)
  liftIO $ mask_ $ do
    atomicWriteIORef rs oldStk -- ^ Important to pop before executing
    rel                        -- (*)
  pure a
