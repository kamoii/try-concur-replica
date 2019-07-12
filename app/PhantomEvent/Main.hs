module Main where

import P
--
import qualified Data.Text              as T
import           Text.Read              (readMaybe)
import           Control.Concurrent.STM (retry, check)
--
import           Concur.Core
import           Concur.Replica

main :: IO ()
main = do
  runDefault 8080 "Phantom Event" $ do
    div []
      [ p [] [ text "Hello" ]
      , button [ onClick ] [ text "nextPage" ]
      ]
