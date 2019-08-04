{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import P hiding (span, id, whenJust)
import Control.Category (id)
import qualified Relude.Extra.Enum as BEnum
import qualified Data.Text              as T
import           Control.Concurrent.STM (retry, check)
import           Control.Concurrent     (threadDelay)
import Control.Lens hiding (zoom, to, re, matching)
import Data.Generics.Labels
import Data.V.Core as V
import Data.V.Text as V
--
import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)
import           Concur.Core
import           Concur.Replica.Extended hiding (id)
import           Concur.Replica.STM
import           Concur.Replica.Control.Misc
import           Concur.Replica.Control.Validation
import qualified Concur.Replica.Control.Exception as E
import           Concur.Replica.Widget.Input
--
import Domain

{-
良く考えたら randomって入れる必要ないな。
どっちかというと「条件が合う人が集まるまで待つ」、だ

+スプラトゥーン2のリグマをやりたい奴が集ってランダムにマッチするサービスです。+


リグマやりたいのにイカ友達がいない・集まらない
twitter で見知らぬイカに声かけるのは抵抗がある
ガチマに疲れた

では、入力画面にgo-

現在β(ベータ)版です。
!ない機能

ログイン機能
チャット・音声通話機能(discordを利用)
スマフォでの動作保証ない。PCで見てね
-}

welcome :: _ => Ctx -> m ()
welcome ctx =
  div []
    [ h1 [] [ t "#リグマ部屋(beta)" ]
    -- このサービスを一言で
    , p [] [ t "スプラトゥーン2でリーグマッチ(通称「リグマ」)の仲間と" ]
    , p [] [ t "条件(ランク帯、通話の有無)を入力してマッチング!" ]
    -- , displaySTM (getWaitingNum ctx) $ \i -> span [] [ t $ show i ]
    , () <$ button [onClick] [ t "参加する" ]
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
  通話ありなし(*これはいるかも)
  雰囲気

-}
inputCondition :: _ => (BaseInfo, MatchingCondition) -> m (BaseInfo, MatchingCondition)
inputCondition initial = do
  inputWithValidation validate initial \e i ->
    -- form [] . one $ fieldset []
    orr
    [ fieldset []
      [ legend [] [ t "条件入力" ]
      , Update <$> orr
        [ label [] [ t "名前" ]
        , zoom i (_1 . #ikaName) $ inputOnChange [ placeholder "例) ぼっち六号" ]
        , label [] [ t "フレンドコード" ]
        , zoom i (_1 . #ikaFriendCode) $ inputOnChange [ placeholder "例) 1234-5678-9012" ]
        , label [] [ t "ランク帯" ]
        , paddingLeft "1rem" $ zoom i (_2 . #mcRankTai) $ radioGroupBEnum rankRender
        , label [] [ t "通話" ]
        , paddingLeft "1rem" $ zoom i (_2 . #mcTuuwa) $ radioGroupBEnum tuuwaRender
        , label [] [ t "使用武器、意気込み等" ]
        , zoom i (_1 . #ikaNote) $ inputOnChange [ placeholder "例) 中射程シューター使いです!" ]
        ]
      ]
    , Done <$ button [ onClick, style [("display", "block"), ("width", "100%"), ("margin", "1rem auto 0 auto")] ] [ t "探す!" ]
    , whenJust e \errs -> ul [] (map (li [] . one . span [style [("color", "red")]] . one . t) errs)
    ]
  where
    rankLabel = \case
      RankCtoB       -> "C- ～ B+"
      RankAtoS       -> "A- ～ S"
      RankSpToX2100  -> "S+ ～ X2100"
      RankAboveX2100 -> "X2100以上"

    rankRender rank radio = do
      div [] . one $ label []
        [ radio []
        , t $ rankLabel rank
        ]

    tuuwaLabel = \case
      TuuwaAri -> "通話あり"
      TuuwaNashi -> "通話なし(テキストチャットのみ)"
      TuuwaEither -> "どちらでも良い"

    tuuwaRender tuuwa radio = do
      div [] . one $ label []
        [ radio []
        , t $ tuuwaLabel tuuwa
        ]

    maxNameLength = 12
    maxNoteLength = 32
    codeRegex = [re|^((sw|SW|ＳＷ)(-|ー)?)?[0-9０-９]{4}(-|ー)?[0-9０-９]{4}(-|ー)?[0-9０-９]{4}$|]

    -- TODO: validation ルールはドメインの話だよね
    validate =
      let
        lessThan' name len = lessThan len !> [ name <> "は" <> show len <> "文字以内に入力してください" ]
        name = to T.strip >>> notBlank !> ["名前は必須です"] >>> lessThan' "名前" maxNameLength
        code = to T.strip >>> notBlank !> [ "フレンドコードは必須です" ] >>> regex codeRegex !> [ "フレンドコードの形式が違います" ]
        note = to T.strip >>> lessThan' "使用武器、意気込み等" maxNoteLength
        bi   = BaseInfo <$> lmapL #ikaName name <*> lmapL #ikaFriendCode code <*> lmapL #ikaNote note
        mc   = MatchingCondition <$> lmapL #mcRankTai id <*> lmapL #mcTuuwa id
        v    = (,) <$> lmap fst bi <*> lmap snd mc
      in pure . applyPure v

    paddingLeft i =
      div [ style [("padding-left", i)] ] . one

{-| マッチング待機画面

 * キャンセル可能

回線が途中で切られることも想定する必要がある。その場合適切にマッチング
候補から外したり、部屋から退出させる必要がある。
-}

data MatchingFailed
  = MFCancel
  | MFTimeout
  deriving Eq

matching
  :: _
  => E.ReleaseStack
  -> Ctx
  -> MatchMember
  -> (Match -> m r)
  -> m (Either MatchingFailed r)
matching rs ctx mem cb = do
  -- TODO: ここで bracket パターンなのはここの責務じゃない気がしますね。
  let acq = startMatching ctx mem
  let rel = \(canceler, _) -> canceler
  E.pbracket rs acq rel $ \(_, matchWait) -> do
      r <- orr
        [ Right <$> liftIO matchWait
        , Left <$> div []
          [ MFTimeout <$ countdown 10 \i -> h1 [] [ t $ show i ]
          , MFCancel <$ button [ onClick ] [ t "cancel" ]
          ]
        ]
      case r of
        Left err    -> pure $ Left err
        Right match -> Right <$> cb match
  where

    -- | カウントダウン
    -- | 0 も一秒間表示されることに注意。その後、() が発火する。
    countdown :: _ => Int -> (forall a. Int -> m a) -> m ()
    countdown i f =
      if i < 0
      then pure ()
      else do
        _ <- liftIO (threadDelay (1 * 1000 * 1000)) <|> f i
        countdown (i-1) f

{-| match画面

現状は即座に部屋に入るでいい気がするな。不満があれば変える。

 * 他の人の状態
 * 他の人のフレコ、部屋立ての人を決定
 * +チャット+ -> discord
 * 退出ボタン(押しまちがいを防ぐ仕組みは必要かな)
-}

matchRoom :: _ => Ctx -> MatchMember -> Match -> m ()
matchRoom ctx mem match = do
  div []
    [ h1 [] [ t "マッチしました!" ]
    , div [] $ map displayMember $ match ^. #matchMembers
    , () <$ button [ onClick ] [ t "部屋を抜ける" ]
    ]
  where
    displayMember mem = do
      div []
        [ h3 [] [ t $ mem ^. #memBaseInfo . #ikaName ]
        , h5 [] [ t $ mem ^. #memBaseInfo . #ikaFriendCode ]
        ]

main :: IO ()
main = do
  let header' = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/awsm.css/dist/awsm.min.css")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/wingcss")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://cdn.jsdelivr.net/gh/kognise/water.css@latest/dist/light.min.css")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/picnic")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://cdnjs.cloudflare.com/ajax/libs/mini.css/3.0.1/mini-default.min.css")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/chota@latest")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/marx-css/css/marx.min.css")])]
  let index = defaultIndex "#リグマ部屋" header'
  let wsopt = defaultConnectionOptions
  ctx <- mkCtx
  run 8080 index wsopt id E.acquire E.release $ \rs -> do
    id <- liftIO $ genId
    orr
      [ header [] [ h3 [] [ t "#リグマ部屋(β)" ] ]
      , main_ []
        [ do
            welcome ctx
            untilRight (initialBaseInfo,initialMc) \i' -> do
              i@(bi, mc) <- inputCondition i'
              let mem = MatchMember id bi mc
              r <- matching rs ctx mem $ matchRoom ctx mem
              case r of
                Left MFTimeout -> pure $ Left i
                Left MFCancel  -> pure $ Left i
                Right _        -> pure $ Left i
        ]
      , footer []
        [ t "@kamoii" ]
      ]
  where
    initialBaseInfo = BaseInfo
      { ikaName = ""
      , ikaFriendCode = ""
      , ikaNote = ""
      }
    initialMc = MatchingCondition
      { mcRankTai = RankAtoS
      , mcTuuwa = TuuwaEither
      }
