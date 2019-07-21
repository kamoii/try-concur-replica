{-# LANGUAGE BlockArguments #-}
module Data.V.Text
  ( module Data.V.Text
  , re
  ) where

import P hiding (span)
import Data.V.Core
--
import qualified Data.Text as T
import qualified Data.Char as C
import Text.Regex.PCRE.Heavy (re, Regex, (=~))

-- | 空白が何を意味するか？
-- | ゼロ個以上の空文字で構成されている場合
notBlank :: V () Text Text
notBlank = fromPred (not . T.all C.isSpace)

-- | 長さ制限
lessThan :: Int -> V () Text Text
lessThan l = fromPred ((<=l) . T.length)

-- | 正規表現による
-- :set -XQuasiQuotes
-- e.g. regex [re|^http.*|]
regex :: Regex -> V () Text Text
regex rgx = fromPred \t -> t =~ rgx
