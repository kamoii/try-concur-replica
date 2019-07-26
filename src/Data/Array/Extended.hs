module Data.Array.Extended
  ( module Data.Array.Extended
  , module Data.Array
  ) where

import P
import Relude.Extra.Enum
import Data.Array


-- | Bounded, Enum ならトータルに安全な Array が作成可能
-- 通用のArrayの場合
benumArray
  :: (Ix i, Bounded i, Enum i)
  => (i -> e)
  -> Array i e
benumArray f =
  listArray (minBound, maxBound) (map f universe)

benumArrayA
  :: (Ix i, Bounded i, Enum i, Applicative m)
  => (i -> m e)
  -> m (Array i e)
benumArrayA = sequenceA . benumArray
