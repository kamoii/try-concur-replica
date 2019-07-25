{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Domain where

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

data Ctx = Ctx

newtype ID = ID Int

data BaseInfo = BaseInfo
  { ikaName :: Text
  , ikaFriendCode :: Text
  , ikaNote :: Text
  } deriving (Generic, Show)

data RankTai
  = RankCtoB         -- C- ~ B+
  | RankAtoS         -- A- ~ S
  | RankSpToX2100    -- S+ ~ X2100
  | RankAboveX2100   -- X2100 ~
  deriving (Eq, Show, Bounded, Enum)

data Tuuwa
  = TuuwaAri
  | TuuwaNashi
  | TuuwaEither
  deriving (Eq, Show, Bounded, Enum)

data MatchingCondition = MatchingCondition
  { mcRankTai :: RankTai
  , mcTuuwa :: Tuuwa
  } deriving (Generic, Show)

data MatchMemberState
  = MSPresent      -- ^ 在籍
  | MSDisappeared  -- ^ 回線切れ
  | MSExited       -- ^ 退出済み

data MatchMember = MatchMember
  { memId :: ID
  , memBaseInfo :: BaseInfo
  , memMatchingCondition :: MatchingCondition
  }

data MatchEvent
  = EVMemberStateChange MatchMember MatchMemberState
  | EVMemberGreeting MatchMember
  | EvComment MatchMember Text

data Match = Match
  { matchMatchMembers :: [(MatchMember, TVar MatchMemberState)]
  , matchEvents :: TVar (Seq MatchEvent)
  , matchRankTai :: RankTai
  }

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
