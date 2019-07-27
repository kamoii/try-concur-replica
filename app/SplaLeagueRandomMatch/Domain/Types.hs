{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Types where

import P
import Data.Array (Ix)
import qualified Relude.Extra.Enum as BEnum
--

newtype ID = ID Int
  deriving (Eq, Ord, Show)

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
  deriving (Ix, Eq, Ord, Show, Bounded, Enum)

data Tuuwa
  = TuuwaAri
  | TuuwaNashi
  | TuuwaEither
  deriving (Eq, Show, Bounded, Enum)

data MatchingCondition = MatchingCondition
  { mcRankTai :: RankTai
  , mcTuuwa :: Tuuwa
  } deriving (Generic, Show)

-- data MatchMemberState
--   = MSPresent      -- ^ 在籍
--   | MSDisappeared  -- ^ 回線切れ
--   | MSExited       -- ^ 退出済み

data MatchMember = MatchMember
  { memId :: ID
  , memBaseInfo :: BaseInfo
  , memMatchingCondition :: MatchingCondition
  }

-- data MatchEvent
--   = EVMemberStateChange MatchMember MatchMemberState
--   | EVMemberGreeting MatchMember
--   | EvComment MatchMember Text

data Match = Match
  { matchMembers :: [MatchMember]
  -- , matchEvents :: TVar (Seq MatchEvent)
  , matchRankTai :: RankTai
  , matchTuuwa :: Tuuwa
  }
