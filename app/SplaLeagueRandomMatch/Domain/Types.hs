{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Types where

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
  , matchTuuwa :: Tuuwa
  }
