module Main where

import P

--
import qualified Data.Text              as T
import           Text.Read              (readMaybe)
import           Control.Concurrent.STM (retry, check)
import           Control.Concurrent (threadDelay)
import           Control.Exception
--
import           Concur.Core
import           Concur.Replica

counter :: Int -> Widget HTML void
counter i = do
  div []
    [ text $ "counter: " <> show i
    , button [ onClick ] [ text "inc" ]
    ]
  counter $ i+1


{-
実装の問題で時偶イベントを失なう
-}
widget :: Int -> Widget HTML void
widget i = do
  liftIO (threadDelay $ 1 * 1000 * 1000) <|> div [] [ text $ show i ]
  widget $ i+1

main :: IO ()
main = do
  runDefault 8080 "test" $ orr
    [ widget 0
    , counter 0
    ]
