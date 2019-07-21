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
import           Control.Monad.Reader
import           Control.Monad.Base
import           Control.ShiftMap
import Control.Lens hiding (zoom)
import Data.Generics.Labels
import Data.Typeable (typeRep)

--
import           Concur.Core
import           Concur.Replica.Extended hiding (id)
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

-- | text input

-- Most of the time you won't need `onInput`, `onChange` is enough.
inputOnChange :: _ => [Props Text] -> Text -> m Text
inputOnChange props txt =
  input $ [ onChange <&> targetValue . target , value txt ] <> props

-- | radio input

radioGroup
  :: _
  => Text                            -- ^ Radio group name(must to be unique)
  -> [(e, Text)]                     -- ^ Optoins and Label text
  -> (m a -> ([Props e] -> m e) -> m e) -- ^ Render function with label and radio widgets as args
  -> e                               -- ^ Current value
  -> m e
radioGroup gname opts f val =
  orr $ opts & map \(e, txt) -> do
    let label = text' txt
    let props = [ type_ "radio", name gname, e <$ onChange, checked (e == val) ]
    let radio props' = input $ props <> props'
    f label radio

-- 簡易版。ほとんどの場合こちらでいいはず
--   * Tpyeable を使って e の型名を name に (重複していないこと保証する必要あり)
--   * Bounded, Enum が必要
radioGroupBEnum
  :: forall e a m._
  => (e -> Text)                      -- ^ Label function
  -> (m a -> ([Props e] -> m e) -> m e) -- ^ Render function with label and radio widgets as args
  -> e                               -- ^ Current value
  -> m e
radioGroupBEnum lf f val =
  radioGroup
    (show $ typeRep $ Proxy @e)
    (map (\e -> (e, lf e)) BEnum.universe)
    f val

-- focus?
-- 引数の順序としては Lens' v a -> (a -> m a) -> v -> mv のほうが綺麗だが
-- 微妙なことに Lens の `zoom` と名前が被っている。
zoom :: Functor m => v -> Lens' v a -> (a -> m a) -> m v
zoom v l f = f (v ^. l) <&> \a -> v & l .~ a

-- untilRight を二回重ねることで validation 付きの form が可能。
-- ただし realtime な validation ではなく、
untilRight :: Monad m => s -> (s -> m (Either s r)) -> m (s, r)
untilRight s f =
  f s >>= either (flip untilRight f) (pure . (s,))

whenJust :: Alternative m => Maybe a -> (a -> m v) -> m v
whenJust m f = case m of
  Just a -> f a
  Nothing -> empty

-- realtime な validation ではない

data Input i = Update i | Done

inputWithValidation
  :: _
  => (i -> m (Either e r))             -- ^ Validate function(Don't show widgets)
  -> i                                -- ^ Initial value
  -> (Maybe e -> i -> m (Input i))      -- ^ Left i はループ、Right () is try to proceed
  -> m r
inputWithValidation validate initial render =
  snd <$> untilRight (Nothing, initial) \(ieMaybe, i) -> do
    v <- fst <$> untilRight i (inputToEither <<$>> render ieMaybe)
    t <- validate v
    pure case t of
      Left e  -> Left (Just e, v)
      Right r -> Right r
  where
    inputToEither (Update i) = Left i
    inputToEither Done = Right ()

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
        name    = field #ikaName       $ notBlank !> ["名前は必須です"] >>> lessThan 20 !> [ "名前は20文字までです。"]
        code    = field #ikaFriendCode $ notBlank !> [ "フレンドコードは必須です" ] >>> lessThan 30 !> [ "20文字までです" ]
        rankTai = field #ikaRankTai      id
        note    = field #ikaNote       $ lessThan 32 !> [ "32文字までです" ]
        v       = Ika <$> name <*> code <*> rankTai <*> note
      in pure . applyV v


main :: IO ()
main = do
  runDefault 8080 "#リグマ" do
    welcome Ctx
    ika <- inputUser
    t $ show ika
