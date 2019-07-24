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
import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)


--
import           Concur.Core
import           Concur.Replica.Extended hiding (id)
import           Concur.Replica.STM
import           Concur.Replica.Control.Misc
import           Concur.Replica.Control.Validation
import qualified Concur.Replica.Control.Exception as E
import           Concur.Replica.Widget.Input

-- for test/development
ioBlock :: IO a
ioBlock = threadDelay (1 * 1000 * 1000 * 1000 * 1000) *> undefined

{-
生存及び死亡の区別を誰が責任持つかだよな。
Context が自分が死ぬ時に自分の後片付けを行なう。多分効率がいいけど、抜けがありそう。
逆に管理thread に自分の生存かいなかを管理できるものを渡してその thread に任せる？
抜けはなさそうだけど効率は悪いかな(thread は前Context の生存状態を監視する必要あり)
-}

data Ctx = Ctx

data BaseInfo = BaseInfo
  { ikaName :: Text
  , ikaFriendCode :: Text
  , ikaNote :: Text
  } deriving (Generic, Show)

data RankTai
  = RankCtoB         -- C- ~ B+
  | RankAtoS         -- A- ~ S
  | RankSpToX2100    -- S+ ~ X2100
  | RankAboveX2100   -- X2100 ~
  deriving (Eq, Show, Bounded, Enum)

data MatchingCondition = MatchingCondition
  { mcRankTai :: RankTai
  } deriving (Generic, Show)

data Match = Match
  { roomMembers :: [MatchingCondition]
  }

getCurrentWaitingNum :: Ctx -> STM Int
getCurrentWaitingNum ctx = pure 4

-- 最初の `IO ()` は canceler。マッチング待ちや、部屋に入った状態をキャンセルする。
-- つまり参加していない状態にする。基本的に部屋に入った状態はキャンセルしたくないが、
-- ちょうどユーザとキャンセルボタンを押した直後にマッチングしてしまった場合など。
--
-- 二番目の `IO Match` はマッチングするまでブロックする。マッチすると Match が返る。
--
-- TODO: canceler というか detacher ? のほうがいいかもな
startMatching :: Ctx -> IO (IO (), IO Match)
startMatching ctx = do
  pure (pure (), ioBlock)

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
  通話ありなし(*これはいるかも)
  雰囲気

-}
inputCondition :: _ => (BaseInfo, MatchingCondition) -> m (BaseInfo, MatchingCondition)
inputCondition initial = do
  inputWithValidation validate initial \e i ->
    div []
      [ whenJust e \errs -> div [] (map t errs)
      , Update <$> zoom i (_1 . #ikaName) (inputOnChange [ placeholder "四号" ])
      , Update <$> zoom i (_1 . #ikaFriendCode) (inputOnChange [ placeholder "1234-5678-9012" ])
      , Update <$> zoom i (_2 . #mcRankTai) (radioGroupBEnum rankRender)
      , Update <$> zoom i (_1 . #ikaNote) (inputOnChange [ placeholder "使用武器、意気込み等" ])
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
        name    = to T.strip >>> notBlank !> ["名前は必須です"] >>> lessThan' "名前" maxNameLength
        code    = to T.strip >>> notBlank !> [ "フレンドコードは必須です" ] >>> regex codeRegex !> [ "形式が違います" ]
        note    = to T.strip >>> lessThan' "意気込み等" maxNoteLength
        bi      = BaseInfo <$> lmapL #ikaName name <*> lmapL #ikaFriendCode code <*> lmapL #ikaNote note
        mc      = MatchingCondition <$> lmapL #mcRankTai id
        v       = (,) <$> lmap fst bi <*> lmap snd mc
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

matching :: _ => E.ReleaseStack -> Ctx -> (BaseInfo, MatchingCondition) -> (Match -> m r) -> m (Either MatchingFailed r)
matching rs ctx ika cb = do
  let acq = startMatching ctx
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

{-| リーグ画面

現状は即座に部屋に入るでいい気がするな。不満があれば

 * 他の人の状態
 * 他の人のフレコ、部屋立ての人を決定
 * チャット
 * 退出ボタン(押しまちがいを防ぐ仕組みは必要かな)
-}

league :: _ => Ctx -> Match -> m ()
league = undefined

main :: IO ()
main = do
  let index = defaultIndex "#リグマ" mempty
  let wsopt = defaultConnectionOptions
  let ctx = Ctx
  run 8080 index wsopt id E.acquire E.release $ \rs -> do
    welcome ctx
    untilRight (initialBaseInfo,initialMc) \i' -> do
      i <- inputCondition i'
      r <- matching rs ctx i \room -> pure ()
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
      }
