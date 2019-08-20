{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Domain
  ( module Domain.Types
  , Ctx
  , mkCtx
  , genId
  , getWaitingNum
  , startMatching
  ) where

import P hiding (span, id, whenJust)
import Control.Lens
import Control.Category (id)
import Data.V.Core as V
import Data.V.Text as V
import qualified Relude.Extra.Enum as BEnum
--
import           Control.Concurrent.STM (throwSTM)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent     (threadDelay)
import           Control.Exception      (mask, throwIO, try)
import           System.Random (randomIO)

import Domain.Types
import qualified Domain.Matching as D
import qualified Domain.Discord as Dis

{- | ドメインコード

ドメインの一貫性を保つのが責務。このモジュールから提供する関数を使っている範囲では、
ドメインの一貫性が損なわれないことを保証する必要がある。

Concur 及び Replica 関連のコードがこの Domainモジュールに混ってはいけない。
-}

{-
生存及び死亡の区別を誰が責任持つかだよな。
Context が自分が死ぬ時に自分の後片付けを行なう。多分効率がいいけど、抜けがありそう。
逆に管理thread に自分の生存かいなかを管理できるものを渡してその thread に任せる？
抜けはなさそうだけど効率は悪いかな(thread は前Context の生存状態を監視する必要あり)
-}

data Attach a v = Attach a v

attachment (Attach a _) = a

instance Eq v => Eq (Attach a v) where
  Attach _ v0 == Attach _ v1 = v0 == v1

instance Ord v => Ord (Attach a v) where
  Attach _ v0 <= Attach _ v1 = v0 <= v1

data Ctx = Ctx
  { ctxQueue :: TVar (D.MatchingQueue (Attach (MatchMember,TMVar Match) ID))
  }

mkCtx :: IO Ctx
mkCtx = do
  q <- newTVarIO D.mkMatchingQueue
  pure $ Ctx q

genId :: IO ID
genId = ID <$> randomIO

-- | 現在の待ち人数を取得する。
getWaitingNum :: Ctx -> STM Int
getWaitingNum Ctx{ctxQueue} = D.waitingNum <$> readTVar ctxQueue

-- | マッチングに参加する
--
-- 最初の `IO ()` は canceler。マッチング待ちや、部屋に入った状態をキャンセルする。
-- つまり参加していない状態にする。基本的に部屋に入った状態はキャンセルしたくないが、
-- ちょうどユーザとキャンセルボタンを押した直後にマッチングしてしまった場合など。
--
-- 二番目の `IO Match` はマッチングするまでブロックする。マッチすると Match が返る。
--
-- TODO: canceler というか detacher ? のほうがいいかもな
startMatching :: Ctx -> Dis.LigumaDiscord -> MatchMember -> IO (IO (), IO Match)
startMatching Ctx{ctxQueue} dis mem = do
  (roomWait, canceler, matchMaybe) <- atomically $ do
    var <- newEmptyTMVar
    let a = Attach (mem, var) (memId mem)
    matchMaybe <- stateTVarM ctxQueue
      $ either throwSTM pure
      . D.addAndTryMatch (a, memMatchingCondition mem)
    pure
      ( atomically $ readTMVar var
      , atomically $ modifyTVar' ctxQueue (D.cancel a)
      , (_1 %~ map attachment) <$> matchMaybe
      )
  -- 上記のSTMでそのまま Match をディスパッチしないのは、Match のため
  -- の Discord のチャンネルを IO で作成する必要があるため。
  case matchMaybe of
    Nothing -> pure ()
    Just v  -> mkMatchAndDispatch v
  pure (canceler, roomWait)
  where
    mkMatchAndDispatch :: ([(MatchMember, TMVar Match)], RankTai, Tuuwa) -> IO ()
    mkMatchAndDispatch (ms,rankTai,tuuwa) = do
      let (members, vars) = unzip ms
      let match = Match members rankTai tuuwa
      atomically $ traverse_ (\v -> tryPutTMVar v match) vars

    stateTVarM :: TVar s -> (s -> STM (a, s)) -> STM a
    stateTVarM var f = do
      (a, s') <- f =<< readTVar var
      writeTVar var s'
      pure a

-- for test/development
ioBlock :: IO a
ioBlock = forever $ threadDelay (1 * 1000 * 1000 * 1000 * 1000)
