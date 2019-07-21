{-# LANGUAGE BlockArguments #-}
module Data.V.Text where

import P hiding (span)
import Data.V.Core
--
import qualified Data.Text as T
import qualified Data.Char as C

-- | 空白が何を意味するか？
-- | ゼロ個以上の空文字で構成されている場合
notBlank :: V () Text Text
notBlank = fromPred (not . T.all C.isSpace)

-- | 長さ制限
lessThan :: Int -> V () Text Text
lessThan l = fromPred ((<=l) . T.length)
