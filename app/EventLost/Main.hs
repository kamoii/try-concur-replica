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

{-|
高速にカウントする自動カウンタと手動カウンタを同時表示したら、
Event lost問題を再現できるかと思ったが、かなり高速にカウントしても
問題なかった。

ということは、気にするほどでもないってことか。僅かな可能性で発生したとしても、
ユーザは「おや？」と思うぐらいか。
-}

manualCounter :: Int -> Widget HTML void
manualCounter i = do
  div []
    [ text $ "counter: " <> show i
    , button [ onClick ] [ text "inc" ]
    ]
  manualCounter $ i+1

autoCounter :: Int -> Widget HTML void
autoCounter i = do
  liftIO (threadDelay $ 1 * 1 * 1000) <|> div [] [ text $ show i ]
  autoCounter $ i+1

main :: IO ()
main = do
  runDefault 8080 "test" $ orr
    [ autoCounter 0
    , manualCounter 0
    ]
