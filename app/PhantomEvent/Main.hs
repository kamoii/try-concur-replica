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


exWidget :: Widget HTML a
exWidget = do
  e <- div [ onClick, error "foo" ] [ text "BOOM" ]
  -- a <- liftIO $ evaluate ((5 `P.div` 0))
  text $ T.pack $ show "Done"
  exWidget

main :: IO ()
main = do
  runDefault 8080 "test" $ do
    exWidget
    -- div []
    --   [ p [] [ text "hello" ]
    --   , button [ onClick ] [ text "go next" ]
    --   ]
    -- c <- div []
    --   [ p [] [ text "choose" ]
    --   , div []
    --     [ button [ "a" <$ onClick ] [ text "a" ]
    --     , button [ "b" <$ onClick ] [ text "b" ]
    --     ]
    --   ]
    -- p [] [ text $ "you choosed: " <> c ]
