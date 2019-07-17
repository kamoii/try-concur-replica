module Main where

import qualified Prelude as P (div)
import P

--
import qualified Data.Text              as T
import           Text.Read              (readMaybe)
import           Control.Concurrent.STM (retry, check)
import           Control.Exception
--
import           Concur.Core
import           Concur.Replica

{-|
ラグがある環境(例: throttle --rt 500 --localhost)だと、"go next"のボタンを二回押す、
二回目のイベントが同じパスを持つ "a" ボタンに発火してしまい、"a" が自動的に選択される。
これが "a" じゃなく、副作用を持つ処理の決定ボタンだと問題。
-}

main :: IO ()
main = do
  runDefault 8080 "test" $ do
    div []
      [ p [] [ text "hello" ]
      , button [ onClick ] [ text "go next" ]
      ]
    c <- div []
      [ p [] [ text "choose" ]
      , button [ "a" <$ onClick ] [ text "a" ]
      , button [ "b" <$ onClick ] [ text "b" ]
      ]
    p [] [ text $ "you choosed: " <> c ]
