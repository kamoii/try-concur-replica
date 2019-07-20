module Concur.Replica.STM where

import P
import           Control.Concurrent.STM (check)
--
import           Concur.Core
import           Concur.Replica


displaySTM :: (MonadIO m, Alternative m, Eq v) => STM v -> (v -> m Void) -> m a
displaySTM act render = forever $ do
  v <- atomically act
  absurd <$> render v
    <|> liftIO (atomically (act >>= check . (/=v)))
