module Main where

import P

--
import qualified Data.Text              as T
import           Text.Read              (readMaybe)
import           Control.Concurrent.STM (retry, check)
import           Control.Concurrent     (threadDelay)
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Base
import           Control.ShiftMap

--
import           Concur.Core
import           Concur.Replica.Extended
import           Concur.Replica.Control.Route

data Route = Home | About

home :: _ => m a
home = do
  div []
    [ h1 [] [ text' "Home" ]
    , p [] [ text' "Welcome! You can play with a counter!" ]
    , manualCounter 0
    , p [] [ text' "If you want to know about us, visit ", link' About "about", text' "." ]
    ]

about :: _ => m a
about = do
  div []
    [ h1 [] [ text' "About" ]
    , p [] [ text'' "Nothing to say. Just go ", link' Home "home", text' "." ]
    ]

manualCounter :: (WidgetConstraints m, MonadBase (Widget HTML) m) => Int -> m a
manualCounter i = do
  div []
    [ text' $ "counter: " <> show i <> " "
    , button [ onClick ] [ text' "inc" ]
    ]
  manualCounter $ i+1

autoCounter :: Int -> Widget HTML a
autoCounter i = do
  liftIO (threadDelay $ 1 * 1000 * 1000) <|> div [] [ text $ "counter: " <> show i <> " (auto inc)"]
  autoCounter $ i+1

main :: IO ()
main = do
  runDefault 8080 "mtl-style routing" $ orr
    [ autoCounter 0
    , manualCounter 0
    , loopRouteT Home $ \case
        Home -> home
        About -> about
    ]
