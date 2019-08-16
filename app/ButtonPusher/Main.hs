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
import qualified Concur.Replica.Control.Exception as E
import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)

-- | Dumbest online game in the world.
--
-- It shows realtime participant number(who are connected to the server),
-- and realtime total count number since the server booted.

main :: IO ()
main = do
  participantNum <- newTVarIO 0
  counter        <- newTVarIO 0
  let req = atomically $ modifyTVar' participantNum (+1)
  let rel = const $ atomically $ modifyTVar' participantNum (+(-1))
  let cnf = mkDefaultConfig' 8080 "Button Pusher" req rel
  run cnf $ \_ -> do
    div []
      [ h1 [] [ text "Button Pusher!" ]
      , p  [] [ text "Dumbest online game in the world. Just enjoy how the counter goes up." ]
      , displayTVar participantNum $ \v -> h3 [] [ text $ "Current Paticipants: " <> show v ]
      , displayTVar counter        $ \v -> h3 [] [ text $ "Counter: " <> show v ]
      , forever $ do
          button [ onClick ] [ text "Increment Counter" ]
          liftIO $ atomically $ modifyTVar' counter (+1)
      , button [ onClick ] [ text "end" ]
      ]

displayTVar :: Eq v => TVar v -> (v -> Widget HTML Void) -> Widget HTML a
displayTVar tvar render = forever $ do
  v <- readTVarIO tvar
  orr
    [ absurd <$> render v
    , liftIO $ atomically $ readTVar tvar >>= check . (/=v)
    ]
