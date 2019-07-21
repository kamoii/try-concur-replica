{-# LANGUAGE BlockArguments #-}
module Data.Vali where

import P hiding (span)
--
import Control.Lens
import Data.Generics.Product
import Data.Generics.Labels

{-| 合成可能な validtion ライブラリ

現状 hackage に上げられている validationライブラリは、Either の
Applicative インスタンスの実装を変えたものか、json/html form に特化し
たライブラリだけが見つかる。

 * 単に
   andThen と and

Appicative (accumlative な)やつだよ!
sequenceA :: Applicative f => t (f a) -> f (t a) 活用できるざ

-- bifunctor
-- functor
--
i, e の位置変える？ Profunctor にするか Bifunctor にするか
profunctor にして e を変換するのは

open sum error?
monadic parameter m?
-}

newtype Vali e i o = Vali { validate :: i -> Either e o }

instance Functor (Vali e i) where
  fmap f (Vali v) = Vali $ f <<$>> v

instance Semigroup e => Applicative (Vali e i) where
  pure = Vali . const . Right
  liftA2 = andAlso

instance Profunctor (Vali e) where
  lmap f (Vali v) = Vali $ v . f
  rmap = fmap

emap :: (e -> e') -> Vali e i o -> Vali e' i o
emap f (Vali v) = Vali $ (first f) <$> v

(<!>) = emap
(&<!>) = flip emap

(<!) :: e' -> Vali e i o -> Vali e' i o
(<!) e v = emap (const e) v

(!>) :: Vali e i o -> e' -> Vali e' i o
(!>) v e = emap (const e) v


-- either
-- 左辺のエラーか右辺のエラー
andThen :: Vali e i o -> Vali e' o o' -> Vali (Either e e') i o'
andThen (Vali v0) (Vali v1) =
  Vali $ first Left <$> v0 >=> first Right <$> v1

-- accumulative either
andAlso :: Semigroup e => (o -> o' -> o'') -> Vali e i o -> Vali e i o' -> Vali e i o''
andAlso f (Vali v0) (Vali v1) = Vali \i ->
  case (v0 i, v1 i) of
    (Left e, Right _)    -> Left e
    (Right _, Left e)    -> Left e
    (Left e0, Left e1)   -> Left $ e0 <> e1
    (Right o0, Right o1) -> Right $ f o0 o1

-- | alias for `sequenceA`.
-- | 結果の t o どうしよう...
satisfyAll :: (Semigroup e, Traversable t) => t (Vali e i o) -> Vali e i (t o)
satisfyAll = sequenceA

-- | Create `Vali` by prediction function. `True` means valid.
mkValiBool :: (i -> Bool) -> Vali () i i
mkValiBool p = Vali \i -> bool (Left ()) (Right i) (p i)
