{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments #-}
module Domain.Discord where

import P hiding (isPrefixOf)
import Control.Lens
import Data.Generics.Labels
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text as T
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM (throwSTM)
import Control.Concurrent.STM.TMVar
import Text.Pretty.Simple

import Discord
import Discord.Types
import qualified Discord.Internal.Rest as DIR
import qualified Discord.Requests as R

-- orhpn...
instance Exception RestCallErrorCode

readDiscordToken :: IO DiscordToken
readDiscordToken = do
  tok <- readFileText "discord.token"
  pure $ DiscordToken $ T.strip tok

newtype DiscordToken = DiscordToken Text
  deriving Eq

newtype DiscordError = DiscordError Text
  deriving Show
instance Exception DiscordError

test = do
  tok <- readDiscordToken
  dis <- startDiscord tok (\_ _ -> pure ())
  -- 現状一つのギルドのみに所属しているはず
  [pguild] <- restCall' dis $ R.GetCurrentUserGuilds
  when (partialGuildName pguild /= "リグマ部屋") $ throwIO (DiscordError "Unexpected guild")
  let gid = partialGuildId pguild
  -- ギルド「リグマ部屋」の全てのチャネルを取得(カテゴリも含まれている)
  chans <- restCall' dis $ R.GetGuildChannels gid
  pPrint chans
  let ligumaCat' = chans
        & map (preview #_ChannelGuildCategory)
        & catMaybes
        & filter ((=="リグマ") . view _3)
  ligumaCatId <- case ligumaCat' of
    [(catId, _, _)] -> pure catId
    _ -> throwIO $ DiscordError "「リグマ」カテゴリが見つかりせんでした"
  -- 全roles
  roles <- restCall' dis $ R.GetGuildRoles gid
  roleEveryone <- find ((=="@everyone") . roleName) roles & throwNothing (DiscordError "No @everyone role")
  -- 全ギルドメンバー
  ms <- listGuildMembersAll dis gid
  pPrint ms
  let topic = "test\nです"
  let name = "c1345223"
  let overwrite = map (mkMemberOverwrite userAllowPermissions 0) ms
  let opts = R.CreateGuildChannelOptsText
        { createGuildChannelOptsTopic = Just topic
        , createGuildChannelOptsUserMessageRateDelay = Nothing
        , createGuildChannelOptsIsNSFW = Nothing
        , createGuildChannelOptsCategoryId = Just ligumaCatId
        }
  throwLeft =<< restCall dis (R.CreateGuildChannel gid name overwrite opts)
  pure ()

throwLeft :: Exception e => Either e a -> IO a
throwLeft = either throwIO pure

throwNothing e = maybe (throwIO e) pure

restCall' :: (FromJSON b, DIR.Request (r b)) => DiscordHandle -> r b -> IO b
restCall' dis req = throwLeft =<< restCall dis req


createLimitedTextChannel
  :: DiscordHandle
  -> GuildId
  -> Text
  -> [GuildMember]
  -> IO Channel
createLimitedTextChannel dis gid name members = do
  let topic = ""
  let overwrite = members & map \mem -> DIR.Overwrite
        { overwriteId = mem & memberUser & userId
        , overwriteType = "member"
        , overwriteAllow = 248896
        , overwriteDeny = 0
        }
  let opts = R.CreateGuildChannelOptsText
        { createGuildChannelOptsTopic = Just topic
        , createGuildChannelOptsUserMessageRateDelay = Nothing
        , createGuildChannelOptsIsNSFW = Nothing
        , createGuildChannelOptsCategoryId = Nothing
        }
  throwLeft =<< restCall dis (R.CreateGuildChannel gid name overwrite opts)

-- | * 権限回りの戦略
-- |
-- |   * ギルドレベル @everyone は触らない
-- |   * カテゴリを一つ作成し、その @everyone は基本全て禁止に
-- |   * 動的に作成するチャネルはカテゴリ以下に付けて、特定のメンバーだけ
-- |     必要な権限のみ allow する
-- |
-- | 2019/08/03
-- | カテゴリの権限について理解が誤っているようだ

-- | テキストチャネル
-- | https://discordapi.com/permissions.html#216128
-- | 次の権限は落している
-- |
-- |   * TTS Message(読みあげ)
-- |   * 外部絵文字
-- |   * 添付ファイル
-- |   * メッセージ管理(他人のメッセージ削除)
-- |
userAllowPermissions = 216128

-- 全権限のbitを立てたもの
-- https://discordapi.com/permissions.html#2146958847
allPermissions = 2146958847

mkMemberOverwrite :: Integer -> Integer -> GuildMember -> Overwrite
mkMemberOverwrite allow deny mem = DIR.Overwrite
  { overwriteId = mem & memberUser & userId
  , overwriteType = "member"
  , overwriteAllow = allow
  , overwriteDeny = deny
  }

mkRoleOverwrite :: Integer -> Integer -> Role -> Overwrite
mkRoleOverwrite allow deny role = DIR.Overwrite
  { overwriteId = roleID role
  , overwriteType = "role"
  , overwriteAllow = allow
  , overwriteDeny = deny
  }


-- `discord-haskell` のデフォルトのI/Fだと使いにくい。基本 discordから
-- のevent-driven が想定されている。
startDiscord
  :: DiscordToken
  -> (DiscordHandle -> Event -> IO ())
  -> IO DiscordHandle
startDiscord (DiscordToken token) onEvent = do
  handleTmvar <- newEmptyTMVarIO
  let opts = def
        { discordToken = token
        , discordOnEvent = onEvent
        , discordOnStart = onStart handleTmvar
        }
  disAsync <- async $ runDiscord opts
  -- onStart が呼ばれる前に discord スレッドが死ぬ可能性があるので。
  atomically $ readTMVar handleTmvar <|> (waitSTM disAsync >>= throwSTM . DiscordError)
  where
    onStart :: TMVar DiscordHandle -> DiscordHandle -> IO ()
    onStart handleTmvar dis =
      atomically $ putTMVar handleTmvar dis

listGuildMembersAll :: DiscordHandle -> GuildId -> IO [GuildMember]
listGuildMembersAll dis gid =
  paginateAll
    (Snowflake 0)
    (\uid gm -> max uid (gm & memberUser & userId))
    (\uid -> throwLeft =<< restCall dis (R.ListGuildMembers gid (R.GuildMembersTiming (Just 1000) (Just uid))))

-- | pagination を持つ APIから全てのデータを取得する
-- | ただし複数回リエクストを行なった場合、途中の増減については考慮されていない。
paginateAll
  :: Monad m
  => a               -- ^ Initial Seed
  -> (a -> v -> a)     -- ^ Update Seed
  -> (a -> m [v])     -- ^ Get values
  -> m [v]           -- ^ All values
paginateAll i update get = go i []
  where
    go i res = do
      r <- get i
      if null r
        then pure res
        else go (foldl' update i r) (res <> r)
