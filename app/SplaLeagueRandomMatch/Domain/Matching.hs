{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Domain.Matching
  ( MatchingError(..)
  , MatchingQueue
  , mkMatchingQueue
  , addAndTryMatch
  , cancel
  ) where

import P
import Control.Concurrent.STM
import Control.Lens
import Data.Generics.Labels
import Data.Array.Extended (Array, benumArray, (!), (//))
import qualified Data.Map as M

import Domain.Types

data MatchingError
  = AlreadyInTheQueue
  deriving (Eq, Show)

instance Exception MatchingError

-- Array は total に作成する必要あり
data MatchingQueue id = MatchingQueue
  { mqQueue :: Array RankTai [(id, MatchingCondition)]
  }

allIds :: Array RankTai [(id, MatchingCondition)] -> [id]
allIds = map fst . foldl' (<>) []

mkMatchingQueue :: Ord id => MatchingQueue id
mkMatchingQueue =
  MatchingQueue (benumArray (const []))

-- 既に同一idで登録している場合は例外を投げる
addAndTryMatch
  :: Ord id
  => (id, MatchingCondition)
  -> MatchingQueue id
  -> Either MatchingError (Maybe [id], MatchingQueue id)
addAndTryMatch v@(id, cond) MatchingQueue{mqQueue} = do
  when (elem id (allIds mqQueue)) $ Left AlreadyInTheQueue
  let rankTai  = mcRankTai cond
  let que      = (mqQueue ! rankTai) <> [v]
  pure $ case simpleMatching que of
    Just (matches, leftovers) -> (Just matches, MatchingQueue (mqQueue // [(rankTai,leftovers)]))
    Nothing                   -> (Nothing, MatchingQueue (mqQueue // [(rankTai,que)]))

cancel :: Ord id => id -> MatchingQueue id -> MatchingQueue id
cancel id MatchingQueue{mqQueue} =
  MatchingQueue $ mqQueue <&> filter ((/=id) . fst)

{- | マッチングアルゴリズム

 * ランク帯が混じることなし
 * 通話あり・なし・どちらでも

つまり条件のものよっては混じらないものと混じるものがある。

 * ランク帯は完全に混じらない
 * 通話に関しては通話あり・なしは交わらないが「どちらでも」はあり・なし両方と交わる

以下のアルゴリズムは上記に特化したものだが、一般化できそう...

--
先頭を優先して、マッチングさせる。

先頭7取り出したら必ず1組はマッチングは発生するはず。ただし先頭がマッチ
に含まれるとは限らない。その上で先頭に近いところ優先してピックアップ。

-}

simpleMatching
  :: forall  id. Eq id
  => [(id, MatchingCondition)]
  -> Maybe ([id], [(id, MatchingCondition)])
simpleMatching ms
  | length ms < 4 = Nothing
  | otherwise = do
      chosen <- chosen'
      let ids = map (view $ _2 . _1) chosen
      let others = filter (\i -> not $ elem (fst i) ids) seven
      pure (ids, others <> tails)
  where
    seven  = take 7 ms
    tuuwaL = _2 . _2 . #mcTuuwa
    ari    = take 4 $ filter ((/=TuuwaNashi) . view tuuwaL) $ zip [0..] seven
    nashi  = take 4 $ filter ((/=TuuwaAri) . view tuuwaL)   $ zip [0..] seven
    chosen'
      | length ari == 4 && length nashi < 4 = Just ari
      | length ari < 4 && length nashi == 4 = Just nashi
      | length ari == 4 && length nashi == 4 = Just if map fst ari < map fst nashi then ari else nashi
      | otherwise = Nothing
    tails  = drop 7 ms
