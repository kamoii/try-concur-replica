module Concur.Replica.Control.Misc where

import P
import Control.Lens (Lens', (^.), (.~))

-- | Generic control

-- untilRight を二回重ねることで validation 付きの form が可能。
-- ただし realtime な validation ではなく、
untilRight :: Monad m => s -> (s -> m (Either s r)) -> m (s, r)
untilRight s f =
  f s >>= either (flip untilRight f) (pure . (s,))

whenJust :: Alternative m => Maybe a -> (a -> m v) -> m v
whenJust m f = case m of
  Just a -> f a
  Nothing -> empty


-- 引数の順序としては `Lens' v a -> (a -> m a) -> v -> mv` のほうが綺麗だが
-- `(a -> m a)` を後ろに持ってきたほうが書きやすい。
-- 微妙なことに Lens の `zoom` と名前が被っている。focus にする？
zoom :: Functor m => v -> Lens' v a -> (a -> m a) -> m v
zoom v l f = f (v ^. l) <&> \a -> v & l .~ a
