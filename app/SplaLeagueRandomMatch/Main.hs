{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Lens hiding (zoom, to)
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


{-|

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

inputUser :: _ => m Ika
inputUser = do
  inputWithValidation validate initial \e i ->
    div []
      [ whenJust e \errs -> div [] (map t errs)
      , Update <$> zoom i #ikaName (inputOnChange [ placeholder "四号" ])
      , Update <$> zoom i #ikaFriendCode (inputOnChange [ placeholder "1234-5678-9012" ])
      , Update <$> zoom i #ikaRankTai (radioGroupBEnum rankLabel rankRender)
      , Update <$> zoom i #ikaNote (inputOnChange [ placeholder "使用武器、意気込み等" ])
      , Done <$ button [ onClick ] [ t "探す!" ]
      ]
  where
    initial = Ika
      { ikaName = ""
      , ikaFriendCode = ""
      , ikaRankTai = RankAtoS
      , ikaNote = ""
      }

    rankLabel = \case
      RankCtoB       -> "C- ~ B+"
      RankAtoS       -> "A- ~ S"
      RankSpToX2100  -> "S+ ~ X2100"
      RankAboveX2100 -> "X2100 <"

    rankRender label radio = do
      div []
        [ label
        , radio []
        ]

    validate =
      let
        name    = field #ikaName       $ to T.strip >>> notBlank !> ["名前は必須です"] >>> lessThan 20 !> [ "名前は20文字までです。"]
        code    = field #ikaFriendCode $ to T.strip >>> notBlank !> [ "フレンドコードは必須です" ] >>> lessThan 30 !> [ "20文字までです" ]
        rankTai = field #ikaRankTai    $ id
        note    = field #ikaNote       $ to T.strip >>> lessThan 32 !> [ "32文字までです" ]
        v       = Ika <$> name <*> code <*> rankTai <*> note
      in pure . applyV v


main :: IO ()
main = do
  runDefault 8080 "#リグマ" do
    welcome Ctx
    ika <- inputUser
    t $ show ika
