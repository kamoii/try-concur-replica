{-# LANGUAGE BlockArguments #-}
module Data.V.Core where

import P hiding (span, id, (.))
--
import Control.Category (Category(id,(.)))
import Control.Lens
-- import Data.Generics.Product
import Data.Generics.Labels

{-| 合成可能な validtion ライブラリ

現状 hackage に上げられている validationライブラリは、Either の
Applicative インスタンスの実装を変えたものか、json/html form に特化し
たライブラリだけが見つかる。

 * Applicative の instance は accumlative
 * Cateogry

sequenceA :: Applicative f => t (f a) -> f (t a) 活用できるざ

-- bifunctor
-- functor
--
i, e の位置変える？ Profunctor にするか Bifunctor にするか
profunctor にして e を変換するのは

Open-sum(variant) error support?
Add monadic parameter m?
Alternative instance?
-}

-- conV or Validate
newtype V e i o = V { applyV :: i -> Either e o }

instance Functor (V e i) where
  fmap f (V v) = V $ f <<$>> v

instance Semigroup e => Applicative (V e i) where
  pure = V . const . Right
  liftA2 = andAlso

instance Profunctor (V e) where
  lmap f (V v) = V $ v . f
  rmap = fmap

instance Category (V e) where
  id = V Right
  (.) = flip andThen

-- !! このライブラリは純粋に validatoin だけじゃないことに注意。
-- !! そのため、例えば前後の無駄な空白を削るなどの処理もOK。

to :: (i -> i') -> V e i i'
to f = V $ Right . f

-- 型パラメータの位置の関係で Bifunctorに出来無いため
emap :: (e -> e') -> V e i o -> V e' i o
emap f (V v) = V $ (first f) <$> v

(<!>) = emap
infixl 4 <!>

-- これさすがに名前微妙か？
(&<!>) = flip emap
infixl 1 &<!>

(<!) :: e' -> V e i o -> V e' i o
(<!) e v = emap (const e) v
infixl 4 <!

(!>) :: V e i o -> e' -> V e' i o
(!>) = flip (<!)
infixl 4 !>

-- うーん、名前しっくりこない
field :: Lens' v i -> V e i o -> V e v o
field l = lmap (view l)

-- either
-- 左辺のエラーか右辺のエラー
andThen' :: V e i o -> V e' o o' -> V (Either e e') i o'
andThen' (V v0) (V v1) =
  V $ first Left <$> v0 >=> first Right <$> v1

-- | Alias for Category's `>>>`
-- TODO: delete?
andThen :: V e i o -> V e o o' -> V e i o'
andThen v0 v1 = andThen' v0 v1 &<!> (either id id)

-- | alias for Applicative's `liftA`
andAlso :: Semigroup e => (o -> o' -> o'') -> V e i o -> V e i o' -> V e i o''
andAlso f (V v0) (V v1) = V \i ->
  case (v0 i, v1 i) of
    (Left e, Right _)    -> Left e
    (Right _, Left e)    -> Left e
    (Left e0, Left e1)   -> Left $ e0 <> e1
    (Right o0, Right o1) -> Right $ f o0 o1

-- | alias for `sequenceA`.
-- | 結果の t o どうしよう...
-- TODO: delete? 多分使わない
-- satisfyAll :: (Semigroup e, Traversable t) => t (V e i o) -> V e i (t o)
-- satisfyAll = sequenceA

-- | Create `V` by prediction function. `True` means valid.
fromPred :: (i -> Bool) -> V () i i
fromPred p = V \i -> bool (Left ()) (Right i) (p i)
