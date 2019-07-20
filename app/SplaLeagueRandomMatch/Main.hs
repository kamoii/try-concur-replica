{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import P hiding (span)

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
import Control.Lens
import Data.Generics.Product
import Data.Generics.Labels

--
import           Concur.Core
import           Concur.Replica.Extended
import           Concur.Replica.STM

-- TODO: quasi-quoter for text'。埋込み可能、改行は br
t :: _ => _
t = text'

data Ctx = Ctx

getCurrentWaitingNum :: Ctx -> STM Int
getCurrentWaitingNum ctx = pure 4

welcome :: _ => Ctx -> m ()
welcome ctx =
  div []
    [ h1 [] [ t "ランダム・リグマ・マッチング" ]
    , p [] [ t "ランダムで、条件の近いイカとマッチングします。" ]
    , displaySTM (getCurrentWaitingNum ctx) $ \i -> span [] [ t $ show i ]
    , p [] [ t "参加します？" ]
    , button [() <$ onClick] [ t "参加する" ]
    ]

-- Most of the time you won't need `onInput`, `onChange` is enough.
inputOnChange :: _ => [Props Text] -> Text -> m Text
inputOnChange props txt =
  input $ [ onChange <&> targetValue . target , value txt ] <> props

inputOnChangeL :: _ => [Props Text] -> v -> Lens' v Text -> m v
inputOnChangeL props v l =
  inputOnChange props (v ^. l) <&> \t -> v & l .~ t


-- untilRight を二回重ねることで validation 付きの form が可能。
-- ただし realtime な validation ではなく、
untilRight :: Monad m => s -> (s -> m (Either s r)) -> m (s, r)
untilRight s f = f s >>= either (flip untilRight f) (pure . (s,))

{-|

 * フレンドコード(※1 マッチ後にのみ表示されます)
 * 名前
 * ランク帯
   - C- ~ B+
   - A- ~ S
   - S+ ~ X200
   - X200 ~
 * 使用武器、意気込みなど(任意)

以下の要素は後でいいかな？
  通話ありなし
  雰囲気

-}
data RankTai
  = RankCtoB        -- C- ~ B+
  | RankAtoS        -- A- ~ S
  | RankSpToX200    -- S+ ~ X200
  | RankAboveX200   -- X200 ~
  deriving (Eq, Show)

data Ika = Ika
  { ikaName :: Text
  , ikaFriendCode :: Text
  , ikaRankTai :: RankTai
  , ikaNote :: Text
  } deriving (Generic, Show)

inputUser :: _ => m Ika
inputUser = do
  i <- untilRight initial \i -> do
    div []
      [ Left <$> inputOnChangeL [ placeholder "四号" ] i #ikaName
      , Left <$> inputOnChangeL [ placeholder "1234-5678-9012" ] i #ikaFriendCode
      , Right <$> button [ () <$ onClick ] [ t "探す!" ]
      ]
  pure $ fst i
  where
    initial = Ika
      { ikaName = ""
      , ikaFriendCode = ""
      , ikaRankTai = RankAtoS
      , ikaNote = ""
      }

main :: IO ()
main = do
  runDefault 8080 "#リグマ" do
    welcome Ctx
    ika <- inputUser
    t $ show ika
