module Main where

import P
--
import qualified Data.Text              as T
import           System.Random          as R
import           Text.Read              (readMaybe)
import           Control.Concurrent.STM (retry, check)
--
import           Concur.Core
import           Concur.Replica

-- | Dumbest online game in the world.
--
-- It shows realtime participant number(who are connected to the server),
-- and realtime total count number since the server booted.

main :: IO ()
main = do
  participantNum <- newTVarIO 0
  counter        <- newTVarIO 0
  runDefault 8080 "Button Pusher" $ forever $ do
    liftIO $ atomically $ modifyTVar' participantNum (+1)
    div []
      [ h1 [] [ text "Button Pusher!" ]
      , p  [] [ text "Dumbest online game in the world. Just enjoy how the counter goes up." ]
      , displayTVar participantNum $ \v -> h3 [] [ text $ "Current Paticipants: " <> show v ]
      , displayTVar counter        $ \v -> h3 [] [ text $ "Counter: " <> show v ]
      , forever $ do
          button [ onClick ] [ text "Increment Counter" ]
          liftIO $ atomically $ modifyTVar' counter (+1)
      ]

displayTVar :: Eq v => TVar v -> (v -> Widget HTML Void) -> Widget HTML a
displayTVar tvar render = forever $ do
  v <- readTVarIO tvar
  orr
    [ absurd <$> render v
    , liftIO $ atomically $ readTVar tvar >>= check . (/=v)
    ]
