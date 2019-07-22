{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
module Concur.Replica.Widget.Input where

import P
import Relude.Extra.Type (typeName)
import qualified Relude.Extra.Enum as BEnum


import Concur.Core
import Concur.Replica.Extended


-- | どのようなスタイルで表示するかはユーザが決める。このモジュールでは
-- | 必要な挙動のみを定義している。
-- | Widget は基本的に型の一番ケツが a -> m a という形になっている。
-- | これは `zoom` が使えることを意識している。

-- | text input

-- Most of the time you won't need `onInput`, `onChange` is enough.
inputOnChange :: _ => [Props Text] -> Text -> m Text
inputOnChange props txt =
  input $ [ onChange <&> targetValue . target , value txt ] <> props

-- | Radio button

radioGroup
  :: _
  => Text                            -- ^ Radio group name(must to be unique)
  -> [e]                             -- ^ Options
  -> (e -> ([Props e] -> m e) -> m e)   -- ^ Render function with option and radio widgets as args
  -> e                               -- ^ Current value
  -> m e
radioGroup gname opts f val =
  orr $ opts & map \e -> do
    let props = [ type_ "radio", name gname, e <$ onChange, checked (e == val) ]
    let radio props' = input $ props <> props'
    f e radio

-- 簡易版。ほとんどの場合こちらでいいはず
--   * Tpyeable を使って e の型名を name に (重複していないこと保証する必要あり)
--   * Bounded, Enum が必要
radioGroupBEnum
  :: forall e m._
  => (e -> ([Props e] -> m e) -> m e)   -- ^ Render function with option and radio widgets as args
  -> e                               -- ^ Current value
  -> m e
radioGroupBEnum f val =
  radioGroup (show $ typeName @e) BEnum.universe f val
