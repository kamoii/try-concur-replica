{-# LANGUAGE BlockArguments #-}
module Concur.Replica.Control.Validation where

import P
import Concur.Replica.Control.Misc

-- realtime な validation ではない
-- このスタイルだと入力型にフィールドが追加されたとしても例外にならん。
-- 若干変更に弱い...
-- zoom がまずいのか？

data Input i = Update i | Done

inputWithValidation
  :: _
  => (i -> m (Either e r))             -- ^ Validate function(Don't show widgets)
  -> i                                -- ^ Initial value
  -> (Maybe e -> i -> m (Input i))      -- ^ Left i はループ、Right () is try to proceed
  -> m r
inputWithValidation validate initial render =
  snd <$> untilRight (Nothing, initial) \(ieMaybe, i) -> do
    v <- fst <$> untilRight i (inputToEither <<$>> render ieMaybe)
    t <- validate v
    pure case t of
      Left e  -> Left (Just e, v)
      Right r -> Right r
  where
    inputToEither (Update i) = Left i
    inputToEither Done = Right ()
