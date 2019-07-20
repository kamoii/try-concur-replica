module Concur.Replica.Extended
  ( module Concur.Replica
  , module Concur.Replica.Extended
  ) where

import P
import Control.Monad.Base
import Control.ShiftMap
--
import Concur.Core
import Concur.Replica hiding (t)
-- t is just t :: Text -> Text. It should be a utility just closed to Concur.Replica

-- | Additional functionalities for `Concur.Replica`
-- | This module re-exports `Concur.Replica`, so you need to only import `Concur.Replica.Extended`

-- | MonadBase instance to use `Widget HTML` as base monad.
-- is this orphan instance??
instance MonadBase (Widget HTML) (Widget HTML) where
  liftBase = identity


-- | Utility function for `ShiftMap`
--
-- ?? Does this implemenation make sense?
shift :: (Alternative m, ShiftMap m' m) => (forall a'. m' a') -> m a
shift m = shiftMap (const m) empty


-- | Polymorphic version of `Concure.Replica.DOM.text`
--
-- Two version. One using `MonadBase`, other using `ShiftMap`
text' :: MonadBase (Widget HTML) m => Text -> m a
text' = liftBase . text

text'' :: (ShiftMap (Widget HTML) m, Alternative m) => Text -> m a
text'' txt = shift (text txt)
