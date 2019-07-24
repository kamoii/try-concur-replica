{-# LANGUAGE BlockArguments #-}
module Data.V.Core where

import P hiding (span, id, (.))
--
import Data.Profunctor.Product
import Control.Category (Category(id,(.)))
import Control.Lens
-- import Data.Generics.Product
-- import Data.Generics.Labels

{-
+やはり Validation e a でいい気がする。+ いややっぱ面倒か？

 * i -> Validatioon e a は Monad であり Profunctor であることを思い出せ
 * Alt (<|>) は except 系結合
 * 後は e をいじるためのヘルパーいくつか？
-}

{-
2019/07/24 破綻した気がする。だから誰もやらんのか？実験は失敗だ

二つの合成方法がある。

 * except-type        Either のように途中だ脱出する
   - Category, Arrow, Alt
 * accumulative-type  Validatoin のように
   - Applicative, Strong?, ProductProfunctor

いい具合に両方を混ぜようとしたが、失敗に終った。

 * Applicative を accumulative にすると Monad は実装不可能
   - これは Validation型を同様の制限
   - V e i i' >=> V e i' e'' で、前者が失敗した場合、execept タイプの合成なら可能だが
     accumulative タイプの場合、後者も実行する必要があるのだが i' の値が得られないので
     詰む
 * Category は except タイプのみ実装可能
   - これは上記と同様の理由で accumaltive タイプは無理
 * Alternative は empty が実装できないので無理

まあここまではいい。Applicative を accumulative に実装して、Cateogyr を except の実装すれば、
(<*>) と (>>>) をそれぞれ accumulative/except 的結合に使える。

さて、 V e i o と V e i' o' を結合して V e (i,i') (o,o') を得たいとする。
基本的には accumlative な結合がほしいところ、だが

 * 上記の演算を提供するのに Arrow系があるが、Arrow は Category をスーパークラス
   として持つため except 系になってしまう(本当??)
 * Profucntor a => Strong a も似たような演算を提供するが Arrow より弱い(本当？)
   https://www.reddit.com/r/haskell/comments/8hkpo7/arrow_is_more_than_strong_and_category/
   関係しているかも..

-> いや、ProductProfunctor なるものが....
https://hackage.haskell.org/package/product-profunctors-0.10.0.0/docs/Data-Profunctor-Product.html#v:-42--42--42--33-
まさしく求めてた accumulative な V e (i,i') (o,o') への結合じゃん

いや、というか Applicative/Profunctor だけでいけるじゃん...
\f g -> (,) <$> lmap fst f <*> lmap snd g
-}

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

Category と Applicative の性質が異なるのはいいのか？
一方は Either-like、もう一方は Validation-like
うーん、生の Either/Validation を >=> で結合していくのとそんなに変わらない？
いや、面倒か？あと emap 系が定義できんよな
-}

-- conV or Validate
newtype V e i o = V { applyV :: i -> Either e o }

instance Functor (V e i) where
  fmap f (V v) = V $ f <<$>> v

instance Profunctor (V e) where
  lmap f (V v) = V $ v . f
  rmap = fmap

instance Semigroup e => Applicative (V e i) where
  pure = V . const . Right
  liftA2 = andAlso

instance Semigroup e => ProductProfunctor (V e) where
  purePP = pure
  (****) = (<*>)

instance Category (V e) where
  id = V Right
  (.) = flip andThen

-- | `lmap` の lens版
-- `generic-lens` との組合せれば、Fooデータ型の特定のフィールド foo に
-- フォーカスした場合、`lmapL #foo` と書けて便利
lmapL :: Profunctor (f e) => Lens' v i -> f e i o -> f e v o
lmapL l = lmap (view l)

-- !! このライブラリは純粋に validatoin だけじゃないことに注意。
-- !! そのため、例えば前後の無駄な空白を削るなどの処理もOK。
-- 名前、`idmap` でもいい気がしてきた。あ、id自体が何もしない map の意味を含んんでるから駄目か...
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
