module Main where

import P
--
import qualified Data.Text              as T
import           System.Random          as R
import           Text.Read              (readMaybe)
--
import           Concur.Core
import           Concur.Replica

inputEnter :: T.Text -> Widget HTML T.Text
inputEnter v = do
  e <- input [ autofocus True, value v, Left <$> onInput, Right <$> onKeyDown ]
  case e of
    Left e  -> inputEnter (targetValue $ target e)
    Right e -> if kbdKey e == "Enter"
      then pure v
      else inputEnter v

-- Hi/Lo Game. Demonstrates simple architecture of a Concur app.
-- Also a good demonstration of how Concur makes IO effects safe at widget transitions (the random number generation).
main :: IO ()
main = runDefault 8080 "HiLo" $ forever $ do
  h1 [] [text "I'm thinking of a number between 1 and 100"]
  <|> (liftIO (R.randomRIO (1,100)) >>= go)
  where
    go :: Int -> Widget HTML ()
    go n = do
      guessStr <- div []
        [ text "Try to guess: "
        , inputEnter ""
        ]
      case readMaybe (T.unpack guessStr) of
        Nothing -> go n
        Just guess -> do
          if | guess <  n -> div [] [text $ T.pack (show guess) <> " - Go High!"] <|> go n
             | guess >  n -> div [] [text $ T.pack (show guess) <> " - Go Low!"] <|> go n
             | otherwise  -> div [] [text $ "You guessed it! The answer was " <> T.pack (show n), button [const () <$> onClick] [text "Play again"]]
