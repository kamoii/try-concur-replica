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

welcome :: _ => Ctx -> m ()
welcome ctx =
  div []
    [ h1 [] [ t "ランダム・リグマ・マッチング" ]
    , p [] [ t "ランダムで、条件の近いイカとマッチングします。" ]
    , displaySTM (getCurrentWaitingNum ctx) $ \i -> span [] [ t $ show i ]
    , p [] [ t "参加します？" ]
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
    div []
      [ whenJust e \errs -> div [] (map t errs)
      , Update <$> div []
        [ zoom i (_1 . #ikaName) $ inputOnChange [ placeholder "四号" ]
        , zoom i (_1 . #ikaFriendCode) $ inputOnChange [ placeholder "1234-5678-9012" ]
        , zoom i (_2 . #mcRankTai) $ radioGroupBEnum rankRender
        , zoom i (_2 . #mcTuuwa) $ radioGroupBEnum tuuwaRender
        , zoom i (_1 . #ikaNote) $ inputOnChange [ placeholder "使用武器、意気込み等" ]
        ]
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

    tuuwaLabel = \case
      TuuwaAri -> "通話あり"
      TuuwaNashi -> "通話なし"
      TuuwaEither -> "どちらでも良い"

    tuuwaRender tuuwa radio = do
      div []
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
        code = to T.strip >>> notBlank !> [ "フレンドコードは必須です" ] >>> regex codeRegex !> [ "形式が違います" ]
        note = to T.strip >>> lessThan' "意気込み等" maxNoteLength
        bi   = BaseInfo <$> lmapL #ikaName name <*> lmapL #ikaFriendCode code <*> lmapL #ikaNote note
        mc   = MatchingCondition <$> lmapL #mcRankTai id <*> lmapL #mcTuuwa id
        v    = (,) <$> lmap fst bi <*> lmap snd mc
      in pure . applyV v

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
  E.pbracket rs acq rel $ \(_, roomWait) -> do
      r <- orr
        [ Right <$> liftIO roomWait
        , Left <$> div []
          [ MFTimeout <$ countdown 10 \i -> h1 [] [ t $ show i ]
          , MFCancel <$ button [ onClick ] [ t "cancel" ]
          ]
        ]
      case r of
        Left err   -> pure $ Left err
        Right room -> Right <$> cb room
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

matchRoom :: _ => Ctx -> Match -> m ()
matchRoom = undefined

main :: IO ()
main = do
  let index = defaultIndex "#リグマ" mempty
  let wsopt = defaultConnectionOptions
  ctx <- mkCtx
  run 8080 index wsopt id E.acquire E.release $ \rs -> do
    id <- liftIO $ genId
    welcome ctx
    untilRight (initialBaseInfo,initialMc) \i' -> do
      i@(bi, mc) <- inputCondition i'
      let mem = MatchMember id bi mc
      r <- matching rs ctx mem \room -> pure ()
      case r of
        Left MFTimeout -> pure $ Left i
        Left MFCancel  -> pure $ Left i
        Right _        -> pure $ Left i

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
