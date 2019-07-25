{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Domain
  ( module Domain.Types
  , module Domain
  ) where

import P hiding (span, id, whenJust)
import Control.Category (id)
import Data.V.Core as V
import Data.V.Text as V
import qualified Relude.Extra.Enum as BEnum
--
import qualified Data.Text              as T
import           Text.Read              (readMaybe)
import           Control.Concurrent.STM (retry, check)
import           Control.Concurrent     (threadDelay)

import Domain.Types

{- | ドメインコード

ドメインの一貫性を保つのが責務。このモジュールから提供する関数を使っている範囲では、
ドメインの一貫性が損なわれないことを保証する必要がある。

Concur 及び Replica 関連のコードがこの Domainモジュールに混ってはいけない。
-}

{- | STM設計

これ難しいな。
入れ子はあまり望ましくないがしかたないか？
最小限がいいのか、最浅がいいのか?
-}
{-
生存及び死亡の区別を誰が責任持つかだよな。
Context が自分が死ぬ時に自分の後片付けを行なう。多分効率がいいけど、抜けがありそう。
逆に管理thread に自分の生存かいなかを管理できるものを渡してその thread に任せる？
抜けはなさそうだけど効率は悪いかな(thread は前Context の生存状態を監視する必要あり)
-}


-- | 現在の待ち人数を取得する。
getCurrentWaitingNum :: Ctx -> STM Int
getCurrentWaitingNum ctx = pure 4

-- | マッチングに参加する
--
-- 最初の `IO ()` は canceler。マッチング待ちや、部屋に入った状態をキャンセルする。
-- つまり参加していない状態にする。基本的に部屋に入った状態はキャンセルしたくないが、
-- ちょうどユーザとキャンセルボタンを押した直後にマッチングしてしまった場合など。
--
-- 二番目の `IO Match` はマッチングするまでブロックする。マッチすると Match が返る。
--
-- TODO: canceler というか detacher ? のほうがいいかもな
startMatching :: Ctx -> IO (IO (), IO Match)
startMatching ctx = do
  pure (pure (), ioBlock)


-- for test/development
ioBlock :: IO a
ioBlock = forever $ threadDelay (1 * 1000 * 1000 * 1000 * 1000)
