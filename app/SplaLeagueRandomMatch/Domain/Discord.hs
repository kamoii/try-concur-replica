{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments #-}
module Domain.Discord where

import P hiding (isPrefixOf)
import Control.Lens
import Control.Lens.Extras (is)
import Data.Generics.Labels
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Map as M
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

{-|

チャンネルの管理。どんどん動的なチャンネルを作成した場合、いずれはリミットに達してしまう。
そのため古いチャンネルは削除する必要がある。だが、チャンネル自体は作成日時を持たない。
そのため以下のいずれかの手段の取る必要がある。

 * 名前に順序をエンコーディング
 * topic に作成時間をエンコーディング
 *「リグマ」以下カテゴリで配置される順序で利用

-}
data LigumaDiscord = LigumaDiscord
  { ldGuildId :: GuildId
  , ldRoleEveryone :: Role
  , ldMembers :: IORef (Map UserId GuildMember)
  , ldLigumaCategoryId :: ChannelId
  , ldLigumaChannels :: IORef (Map ChannelId Channel)
  , ldAsyncMain :: Async Text
  , ldAsyncEvent :: Async Void
  } deriving Generic

initialize :: IO LigumaDiscord
initialize = do
  tok              <- readDiscordToken
  evMvar           <- newEmptyMVar
  (dis, asyncMain) <- startDiscord tok \_ ev -> putMVar evMvar ev
  pguild'          <- restCall' dis R.GetCurrentUserGuilds
  pguild           <- onlyOne pguild' & throwNothing (DiscordError "予期せぬギルドに所属")
  when (partialGuildName pguild /= "リグマ部屋") $ throwIO (DiscordError "予期せぬギルドに所属")
  let gid          = partialGuildId pguild
  chans            <- restCall' dis $ R.GetGuildChannels gid
  let cats         = chans & map (preview #_ChannelGuildCategory) & catMaybes
  ligumaCategoryId <- view _1 <$> find ((=="リグマ") . view _3) cats & throwNothing (DiscordError "「リグマ」カテゴリが見つかりせんでした")
  let ligumaChans  = chans & filter (belongsToCat ligumaCategoryId)
  channelsRef      <- newIORef mempty
  modifyIORef' channelsRef $ M.union $ M.fromList $ map (\c -> (channelId c, c)) ligumaChans
  roles            <- restCall' dis $ R.GetGuildRoles gid
  roleEveryone     <- find ((=="@everyone") . roleName) roles & throwNothing (DiscordError "No @everyone role")
  membersRef       <- do
    ms  <- listGuildMembersAll dis gid
    var <- newIORef mempty
    modifyIORef' var $ M.union $ M.fromList $ map (\m -> (m & memberUser & userId, m)) ms
    pure var
  asyncEvent       <- async
    $ forever
    $ takeMVar evMvar >>= onEvent gid ligumaCategoryId membersRef channelsRef
  -- link async's ???
  pure $ LigumaDiscord
    { ldGuildId = gid
    , ldRoleEveryone = roleEveryone
    , ldMembers = membersRef
    , ldLigumaCategoryId = ligumaCategoryId
    , ldLigumaChannels = channelsRef
    , ldAsyncMain = asyncMain
    , ldAsyncEvent = asyncEvent
    }
  where
    onlyOne [a] = Just a
    onlyOne _   = Nothing

    belongsToCat :: ChannelId -> Channel -> Bool
    belongsToCat catId = \case
      ChannelText  {channelParentId = Just catId} -> True
      ChannelVoice {channelParentId = Just catId} -> True
      _ -> False

    onEvent guildId catId membersRef channelsRef = \case
      ChannelCreate ch | belongsToCat catId ch             -> pure ()
      ChannelUpdate ch | belongsToCat catId ch             -> pure ()
      ChannelDelete ch | belongsToCat catId ch             -> pure ()
      GuildMemberAdd gid mem | gid == guildId               -> pure ()
      GuildMemberUpdate gid _roles user _a | gid == guildId -> pure ()
      GuildMemberRemove gid user | gid == guildId           -> pure ()
      GuildMemberChunk gid mems | gid == guildId            -> pure () -- ??
      _ -> pure ()



-- pPrint ms
-- let topic = "test\nです"
-- let name = "c1345223"
-- let overwrite
--       = mkRoleOverwrite 0 allPermissions roleEveryone
--       : map (mkMemberOverwrite userAllowPermissions 0) ms
-- let opts = R.CreateGuildChannelOptsText
--       { createGuildChannelOptsTopic = Just topic
--       , createGuildChannelOptsUserMessageRateDelay = Nothing
--       , createGuildChannelOptsIsNSFW = Nothing
--       , createGuildChannelOptsCategoryId = Just ligumaCatId
--       }
-- throwLeft =<< restCall dis (R.CreateGuildChannel gid name overwrite opts)

throwLeft :: Exception e => Either e a -> IO a
throwLeft = either throwIO pure

throwNothing :: Exception e => e -> Maybe a -> IO a
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
-- |   * 動的に作成するチャネルは、@everyone に対して全不許可にし、
-- |     特定のメンバーだけ必要な権限のみ allow する
-- |
-- | 2019/08/03
-- | カテゴリの権限について理解が誤っているようだ


-- 2019/08/03時点で guild を作成したときに付いている権限
-- https://discordapi.com/permissions.html#37215296
userAllowPermissions = 37215296

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
  -> IO (DiscordHandle, Async Text)
startDiscord (DiscordToken token) onEvent = do
  handleTmvar <- newEmptyTMVarIO
  let opts = def
        { discordToken = token
        , discordOnEvent = onEvent
        , discordOnStart = onStart handleTmvar
        }
  disAsync <- async $ runDiscord opts
  -- onStart が呼ばれる前に discord スレッドが死ぬ可能性があるので。
  disHandle <- atomically $ readTMVar handleTmvar <|> (waitSTM disAsync >>= throwSTM . DiscordError)
  pure (disHandle, disAsync)
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
