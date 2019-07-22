{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import P hiding (span, id, whenJust)
import Control.Category (id)
import Data.V.Core as V
import Data.V.Text as V
import qualified Relude.Extra.Enum as BEnum

--
import qualified Data.Text              as T
import           Text.Read              (readMaybe)
import           Control.Concurrent.STM (retry, check)
import           Control.Concurrent     (threadDelay)
import           Control.Exception
import           Control.Monad.Except
import Control.Lens hiding (zoom, to, re, matching)
import Data.Generics.Labels
import Data.Typeable (typeRep)

--
import           Concur.Core
import           Concur.Replica.Extended hiding (id)
import           Concur.Replica.STM
import           Concur.Replica.Control.Misc
import           Concur.Replica.Control.Validation
import           Concur.Replica.Widget.Input

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


{-| 入力画面

 * フレンドコード(※1 マッチ後にのみ表示されます)
 * 名前
 * ランク帯
   - C- ~ B+
   - A- ~ S
   - S+ ~ X2100
   - X2100 ~
 * 使用武器、意気込みなど(任意)

以下の要素は後でいいかな？
  通話ありなし
  雰囲気

-}
data RankTai
  = RankCtoB         -- C- ~ B+
  | RankAtoS         -- A- ~ S
  | RankSpToX2100    -- S+ ~ X2100
  | RankAboveX2100   -- X2100 ~
  deriving (Eq, Show, Bounded, Enum)

data Ika = Ika
  { ikaName :: Text
  , ikaFriendCode :: Text
  , ikaRankTai :: RankTai
  , ikaNote :: Text
  } deriving (Generic, Show)

inputUser :: _ => Ika -> m Ika
inputUser initial = do
  inputWithValidation validate initial \e i ->
    div []
      [ whenJust e \errs -> div [] (map t errs)
      , Update <$> zoom i #ikaName (inputOnChange [ placeholder "四号" ])
      , Update <$> zoom i #ikaFriendCode (inputOnChange [ placeholder "1234-5678-9012" ])
      , Update <$> zoom i #ikaRankTai (radioGroupBEnum rankRender)
      , Update <$> zoom i #ikaNote (inputOnChange [ placeholder "使用武器、意気込み等" ])
      , Done <$ button [ onClick ] [ t "探す!" ]
      ]
  where
    rankLabel = \case
      RankCtoB       -> "C- ~ B+"
      RankAtoS       -> "A- ~ S"
      RankSpToX2100  -> "S+ ~ X2100"
      RankAboveX2100 -> "X2100 <"

    rankRender rank radio = do
      div []
        [ t $ rankLabel rank
        , radio []
        ]

    maxNameLength = 12
    maxNoteLength = 32
    codeRegex = [re|^((sw|SW|ＳＷ)(-|ー)?)?[0-9０-９]{4}(-|ー)?[0-9０-９]{4}(-|ー)?[0-9０-９]{4}$|]

    validate =
      let
        lessThan' name len = lessThan len !> [ name <> "は" <> show len <> "文字以内に入力してください" ]
        name    = lmapL #ikaName       $ to T.strip >>> notBlank !> ["名前は必須です"] >>> lessThan' "名前" maxNameLength
        code    = lmapL #ikaFriendCode $ to T.strip >>> notBlank !> [ "フレンドコードは必須です" ] >>> regex codeRegex !> [ "形式が違います" ]
        rankTai = lmapL #ikaRankTai    $ id
        note    = lmapL #ikaNote       $ to T.strip >>> lessThan' "意気込み等" maxNoteLength
        v       = Ika <$> name <*> code <*> rankTai <*> note
      in pure . applyV v

{-| マッチング待機画面

 * キャンセル可能
-}

data WaitingResult
  = WCancel
  | WTimeout
  | WMacted
  deriving Eq

matching :: _ => Ctx -> Ika -> m WaitingResult
matching ctx ika = do
  div []
    [ WTimeout <$ countdown 10 \i -> t $ show i
    , WCancel <$ button [ onClick ] [ t "cancel" ]
    ]

-- | カウントダウン
-- | 0 も一秒間表示されることに注意。その後、() が発火する。
countdown :: _ => Int -> (forall a. Int -> m a) -> m ()
countdown i f =
  if i < 0
  then pure ()
  else do
    _ <- liftIO (threadDelay (1 * 1000 * 1009)) <|> f i
    countdown (i-1) f

main :: IO ()
main = do
  let ctx = Ctx
  runDefault 8080 "#リグマ" do
    welcome ctx
    untilRight initial \i' -> do
      i <- inputUser i'
      r <- matching ctx i
      case r of
        WTimeout -> pure $ Left i
        WCancel  -> pure $ Left i

  where
    initial = Ika
      { ikaName = ""
      , ikaFriendCode = ""
      , ikaRankTai = RankAtoS
      , ikaNote = ""
      }
