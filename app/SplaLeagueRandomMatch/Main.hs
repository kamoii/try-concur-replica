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
import           Concur.Replica.Extended hiding (id, text)
import           Concur.Replica.STM
import           Concur.Replica.Control.Misc
import           Concur.Replica.Control.Validation
import qualified Concur.Replica.Control.Exception as E
import           Concur.Replica.Widget.Input
--
import Domain
import qualified Domain.Discord as Dis
import NeatInterpolation

{-
良く考えたら randomって入れる必要ないな。
どっちかというと「条件が合う人が集まるまで待つ」、だ


リグマやりたいのにイカ友達がいない・集まらない
twitter で見知らぬ人に声かけるのは抵抗がある
ガチマに疲れた

では、入力画面にgo-

現在β(ベータ)版です。
!ない機能

ログイン機能
チャット・音声通話機能(discordを利用)
スマフォでの動作保証ない。PCで見てね
-}

welcome :: _ => m ()
welcome =
  div []
    [ h1 [] [ t "#リグマ部屋(β)" ]
    , p [] [ t "スプラトゥーン2のリーグマッチ(通称「リグマ」)を一緒に遊ぶ人を探すためのサービスです。" ]

    , p [] . one . t
      $ "掲示板のように募集を行なうものではなく、条件(ランク帯、通話の有無)を入力して待っていれば、"
      <> "同条件の人が集り次第システムが自動的に専用の Discord チャンネルの作成します。"

    , button [ () <$ onClick, style btnStyle ] [ t "条件を入力する" ]

    -- , p [ style [("margin-top", "2rem")] ] . one . strong [] . one . t
    --   $ "❗現在まだベータ版です。見ているブラウザやサーバの状況によっては動作が不安定な可能性があります。"
    ]
  where
    btnStyle = [("dislay", "block"), ("width", "100%"), ("background-color", "#a0d8ef")]


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

data BaseInfoInput = BaseInfoInput
  { biiDiscordUser :: Text            -- ^ e.g. foo#3124
  , biiFriendCode :: Text
  , biiNote :: Text
  } deriving Generic

inputCondition
  :: _
  => Dis.LigumaDiscord
  -> (BaseInfoInput, MatchingCondition)
  -> m (BaseInfoInput, BaseInfo, MatchingCondition)
inputCondition dis initial = do
  inputWithValidation validate initial \e i ->
    -- form [] . one $ fieldset []
    orr
    [ fieldset []
      [ legend [] [ t "条件入力" ]
      , Update <$> orr
        [ label [] [ t "DiscordユーザID" ]
        , br []
        , small []
          [ t "通話・チャットには Discord を利用します。まだ「リグマ部屋」のメンバーではない場合、先に"
          , a [ href "" ] [ t "" ]
          , t "にアクセスしてメンバーになる必要があります。"
          ]
        , zoom i (_1 . #biiDiscordUser) $ inputOnChange [ placeholder "例) botti#5628" ]
        , label [] [ t "フレンドコード" ]
        , zoom i (_1 . #biiFriendCode) $ inputOnChange [ placeholder "例) 1234-5678-9012" ]
        , label [] [ t "ランク帯" ]
        , paddingLeft "1rem" $ zoom i (_2 . #mcRankTai) $ radioGroupBEnum rankRender
        , label [] [ t "通話" ]
        , paddingLeft "1rem" $ zoom i (_2 . #mcTuuwa) $ radioGroupBEnum tuuwaRender
        -- まあ別に不要か取りあえず
        -- , label [] [ t "使用武器、意気込み等" ]
        -- , zoom i (_1 . #biiNote) $ inputOnChange [ placeholder "例) 中射程シューター使いです!" ]
        ]
      ]
    , button
      [ Done <$ onClick, style [("display", "block"), ("width", "100%"), ("background-color", "#a0d8ef")] ]
      [ t "参加する!" ]
    , whenJust e \errs -> ul [] (map (li [] . one . span [style [("color", "red")]] . one . t) errs)
    ]
  where
    rankRender rank radio = do
      div [] . one $ label []
        [ radio []
        , t $ rankLabel rank
        ]

    tuuwaRender tuuwa radio = do
      div [] . one $ label []
        [ radio []
        , t $ tuuwaLabel tuuwa
        ]

    maxNameLength = 12
    maxNoteLength = 32
    codeRegex = [re|^((sw|SW|ＳＷ)(-|ー)?)?[0-9０-９]{4}(-|ー)?[0-9０-９]{4}(-|ー)?[0-9０-９]{4}$|]

    -- TODO: validation ルールはドメインの話だよね
    validate
      :: _
      => (BaseInfoInput, MatchingCondition)
      -> m (Either [Text] (BaseInfoInput, BaseInfo, MatchingCondition))
    validate =
      let
        lessThan' name len = lessThan len !> [ name <> "は" <> show len <> "文字以内に入力してください" ]
        -- name = to T.strip >>> notBlank !> ["名前は必須です"] >>> lessThan' "名前" maxNameLength
        vDis  = fromEither $ maybeToRight ["サーバのメンバーではありません。"] <<$>> (liftIO <$> Dis.lookupMember dis)
        vCode = to T.strip >>> notBlank !> [ "フレンドコードは必須です" ] >>> regex codeRegex !> [ "フレンドコードの形式が違います" ]
        vNote = to T.strip >>> lessThan' "使用武器、意気込み等" maxNoteLength
        vBi   = BaseInfo <$> to biiDiscordUser <*> lmapL #biiDiscordUser vDis <*> lmapL #biiFriendCode vCode <*> lmapL #biiNote vNote
        vMc   = MatchingCondition <$> lmapL #mcRankTai id <*> lmapL #mcTuuwa id
        v     = (,,) <$> to fst <*> lmap fst vBi <*> lmap snd vMc
      in applyM v

    paddingLeft i =
      div [ style [("padding-left", i)] ] . one

rankLabel = \case
  RankCtoB       -> "C- ～ B+"
  RankAtoS       -> "A- ～ S"
  RankSpToX2100  -> "S+ ～ X2100"
  RankAboveX2100 -> "X2100以上"

tuuwaLabel = \case
  TuuwaAri -> "通話あり"
  TuuwaNashi -> "通話なし(チャットのみ)"
  TuuwaEither -> "どちらでも良い"

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
  -> Dis.LigumaDiscord
  -> MatchingCondition
  -> MatchMember
  -> (Match -> m r)
  -> m (Either MatchingFailed r)
matching rs ctx dis mc mem cb = do
  -- TODO: ここで bracket パターンなのはここの責務じゃない気がしますね。
  let acq = startMatching ctx dis mem
  let rel = \(canceler, _) -> canceler
  E.pbracket rs acq rel $ \(_, matchWait) -> do
      r <- orr
        [ Right <$> liftIO matchWait
        , Left <$> div []
          [ p [ style [("margin-top", "2rem")]] [ t "メンバーが集まるのを待っています。" ]
          , dl []
            [ dt [] [ t "ランク帯" ], dd [] [ t $ rankLabel $ mcRankTai mc ]
            , dt [] [ t "通話" ], dd [] [ t $ tuuwaLabel $ mcTuuwa mc ]
            ]
          , MFTimeout <$ countdown 100 \i ->
              orr
              [ t "あと "
              , h2 [ style [("display", "inline-block")] ] [ t $ show i ]
              , t " 秒待ちます..."
              ]
          , div
            [ style [("margin-top", "1rem")] ]
            [ button [ MFCancel <$ onClick ] [ t "キャンセル" ] ]
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

 * 他の人のフレコ、部屋立ての人を決定
 * 退出ボタン(押しまちがいを防ぐ仕組みは必要かな)

-}

matchRoom :: _ => Ctx -> MatchMember -> Match -> m ()
matchRoom ctx self Match{..} = do
  section []
    [ h3 [] [ t "メンバーが集りました!" ]
    , div [] [ b' "ランク帯: ", t (rankLabel matchRankTai <> ", "), b' "通話: ", t (tuuwaLabel matchTuuwa) ]
    , div []
      [ b' "Discordチャンネルに参加お願いします:"
      , br []
      , a [ target_ "_blank",  href matchTextChannelUrl ] [ t $ "#" <> matchRoomName ]
      ]
    , div [ style [("margin-bottom", "1rem")] ]
      [ t "メンバー"
      , ol [] $ map (\m -> li [] [ displayMember m ]) matchMembers
      , small [] [ t "部屋立ては1番がお願いします。2, 3, 4 番は1番にフレンド申請お願いします。" ]
      ]
    , () <$ button [ onClick ] [ t "条件入力画面に戻る" ]
    ]
  where
    b' txt = b [] [ t txt ]
    displayMember MatchMember{memBaseInfo = BaseInfo{..}} =
      t $ ikaDiscordUser <> " / " <> ikaFriendCode

main :: IO ()
main = do
  let header' = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/awsm.css/dist/awsm.min.css")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/wingcss")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://cdn.jsdelivr.net/gh/kognise/water.css@latest/dist/light.min.css")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/picnic")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://cdnjs.cloudflare.com/ajax/libs/mini.css/3.0.1/mini-default.min.css")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/chota@latest")])]
  -- let header = [VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/marx-css/css/marx.min.css")])]
  ctx <- mkCtx
  dis <- Dis.initialize
  let cnf' = mkDefaultConfig' 8080 "#リグマ部屋" E.acquire E.release
  let cnf = cnf' { cfgHeader = header' }
  run cnf \rs -> do
     orr
       [ header_
       , main_ []
         [ do
             -- Discord回りでスレッドが止まってしまった時点でエラー画面を表示する
             -- TODO: 再起動するべきかな？
             _ <- liftIO (atomically $ Dis.waitDeadSTM dis) <|> routeStart rs ctx dis
             t "サーバエラーが発生しました"
         ]
       , footer_
       ]
  where
    -- Main route
    routeStart :: _ => _ -> _ -> _ -> m a
    routeStart rs ctx dis = do
      id <- liftIO $ genId
      welcome
      untilRight (initialBaseInfo,initialMc) \i' -> do
          (bii, bi, cond) <- inputCondition dis i'
          let mem = MatchMember id bi cond
          r <- matching rs ctx dis cond mem $ matchRoom ctx mem
          case r of
            Left MFTimeout -> pure $ Left (bii, cond)
            Left MFCancel  -> pure $ Left (bii, cond)
            Right _        -> pure $ Left (bii, cond)
      t "the end."

    initialBaseInfo = BaseInfoInput
      { biiDiscordUser = ""
      , biiFriendCode = ""
      , biiNote = ""
      }

    initialMc = MatchingCondition
      { mcRankTai = RankAtoS
      , mcTuuwa = TuuwaEither
      }

    header_ = header
      []
      [ strong [] [ t "#リグマ部屋(β)" ] ]

    footer_ = footer
      [ style [("margin-top", "2rem"), ("padding", "0.5rem"), ("border-top", "2px solid #d3d3d3"), ("background-color", "#f5f5f5")] ]
      [ small [] [ t "v0.0.1" ]
      , br []
      , link_ "https://twitter.com/kamoii" "kamoii@twitter"
      , br []
      , link_ "https://twitter.com/kamoii" "リグマ部屋@discord"
      ]
      where
        link_ url txt = a
          [ href url, style [("text-decoration", "none")] ]
          [ small [] [ t txt ] ]
