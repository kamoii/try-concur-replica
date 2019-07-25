{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Domain.Matching where

import P
import Control.Lens
import Data.Generics.Labels

import Domain.Types


{- | マッチングアルゴリズム

 * ランク帯が混じることなし
 * 通話あり・なし・どちらでも

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
