{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments #-}
module Domain.Discord
  ( LigumaDiscord
  , initialize
  , lookupMember
  , mkChannels
  -- re-exports
  , UserId
  ) where

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
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID

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
  { ldHandle           :: DiscordHandle
  , ldGuildId          :: GuildId
  , ldRoleEveryone     :: Role
  , ldMembers          :: IO (Map UserId GuildMember)
  , ldLigumaCategoryId :: ChannelId
  , ldLigumaChannels   :: IO (Map ChannelId Channel)
  , ldAsyncMain        :: Async Text
  , ldAsyncEvent       :: Async Void
  } deriving Generic

initialize :: IO LigumaDiscord
initialize = do
  tok <- readDiscordToken
  evMvar <- newEmptyMVar
  (dis, asyncMain) <- startDiscord tok \_ ev -> putMVar evMvar ev
  gid <- do
    pg' <- restCall' dis R.GetCurrentUserGuilds
    pg  <- onlyOne pg' & throwNothing (DiscordError "予期せぬギルドに所属")
    when (partialGuildName pg /= "リグマ部屋") $ throwIO (DiscordError "予期せぬギルドに所属")
    pure $ partialGuildId pg
  (ligumaCategoryId, channelsRef) <- do
    cs <- restCall' dis $ R.GetGuildChannels gid
    let cats = catMaybes $ map (preview #_ChannelGuildCategory) cs
    lcatId <- view _1 <$> find ((=="リグマ") . view _3) cats & throwNothing (DiscordError "「リグマ」カテゴリが見つかりせんでした")
    let ligumaChans = filter (belongsToCat lcatId) cs
    var <- newIORef mempty
    modifyIORef' var $ M.union $ M.fromList $ map (\c -> (channelId c, c)) ligumaChans
    pure $ (lcatId, var)
  roleEveryone <- do
    rs <- restCall' dis $ R.GetGuildRoles gid
    find ((=="@everyone") . roleName) rs & throwNothing (DiscordError "No @everyone role")
  membersRef <- do
    ms  <- listGuildMembersAll dis gid
    var <- newIORef mempty
    modifyIORef' var $ M.union $ M.fromList $ map (\m -> (m & memberUser & userId, m)) ms
    pure var
  asyncEvent <- async
    $ forever
    $ takeMVar evMvar >>= onEvent gid ligumaCategoryId membersRef channelsRef
  link2 asyncMain asyncEvent
  pure $ LigumaDiscord
    { ldHandle = dis
    , ldGuildId = gid
    , ldRoleEveryone = roleEveryone
    , ldMembers = readIORef membersRef            -- ^ ref を更新するのは eventハンドラだけ
    , ldLigumaCategoryId = ligumaCategoryId
    , ldLigumaChannels = readIORef channelsRef    -- ^ ref を更新するのは eventハンドラだけ
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

    onEvent guildId catId membersRef channelsRef ev = do
      let updateChns = modifyIORef' channelsRef
      let updateMems = modifyIORef' membersRef
      case ev of
        ChannelCreate ch
          | belongsToCat catId ch -> updateChns $ set (at $ channelId ch) (Just ch)
        ChannelUpdate ch
          | belongsToCat catId ch -> updateChns $ set (at $ channelId ch) (Just ch)
        ChannelDelete ch
          | belongsToCat catId ch -> updateChns $ set (at $ channelId ch) Nothing
        GuildMemberAdd gid mem
          | gid == guildId         -> updateMems $ set (at $ userId $ memberUser $ mem) (Just mem)
        GuildMemberUpdate gid roles user nick
          | gid == guildId         -> updateMems $ over (at $ userId user) $ \mem' -> mem' <&> \mem -> mem { memberUser = user, memberNick = nick, memberRoles = roles }
        GuildMemberRemove gid user
          | gid == guildId         -> updateMems $ set (at $ userId user) Nothing
        GuildMemberChunk gid mems
          | gid == guildId         -> pPrint mems *> pure () -- ??
        _ -> pure ()

-- | username + '#' + descriminator から メンバーの UserId を取得する
lookupMember :: LigumaDiscord -> Text -> IO (Maybe UserId)
lookupMember LigumaDiscord{ldMembers} ud = do
  users <- map memberUser . M.elems <$> ldMembers
  pure $ userId <$> find (\u -> userName u <> "#" <> userDiscrim u == ud) users

-- | テキストチャンネルの作成
-- | 名前は一意的なものが勝手に振られる。
-- | 招待用のURLが返される
-- |
mkChannels
  :: LigumaDiscord
  -> [UserId]
  -> Text                -- ^ 名前(一意性の保証不要、だがあった方がいい)
  -> Text                -- ^ テキストチャンネルの冒頭の挨拶
  -> Bool                -- ^ 音声もありか
  -> IO Text             -- ^ テキストチャンネルのURL
mkChannels LigumaDiscord{ .. } users name greeting voiceToo = do
  members <- ldMembers
  mems <- traverse (\uid -> members ^. at uid) users
    & throwNothing (DiscordError "この中にギルド脱退者が居る")
  now <- getPOSIXTime
  let topic = show now
  let overwrite
        = mkRoleOverwrite 0 allPermissions ldRoleEveryone
        : map (mkMemberOverwrite userAllowPermissions 0) mems
  ch <- restCall' ldHandle $ R.CreateGuildChannel ldGuildId name overwrite (txtOpts topic ldLigumaCategoryId)
  _  <- when voiceToo $ void $ restCall' ldHandle
                          $ R.CreateGuildChannel ldGuildId name overwrite (voiceOpts ldLigumaCategoryId)
  _  <- restCall' ldHandle $ R.CreateMessage (channelId ch) greeting
  -- iv <- restCall' ldHandle $ R.CreateChannelInvite (channelId ch) ivOpts
  pure $ textChannelUrl ch
  where
    txtOpts topic catId = R.CreateGuildChannelOptsText
      { createGuildChannelOptsTopic = Just topic
      , createGuildChannelOptsUserMessageRateDelay = Nothing
      , createGuildChannelOptsIsNSFW = Nothing
      , createGuildChannelOptsCategoryId = Just catId
      }

    voiceOpts catId = R.CreateGuildChannelOptsVoice
      { createGuildChannelOptsBitrate = Nothing
      , createGuildChannelOptsMaxUsers = Nothing
      , createGuildChannelOptsCategoryId = Just catId
      }

    -- 招待は 30分だけ有効とする
    ivOpts = R.ChannelInviteOpts
      { channelInviteOptsMaxAgeSeconds = Just (30 * 60)
      , channelInviteOptsMaxUsages = Nothing
      , channelInviteOptsIsTemporary = Nothing
      , channelInviteOptsDontReuseSimilarInvite = Nothing
      }

textChannelUrl :: Channel -> Text
textChannelUrl ch =
  "https://discordapp.com/channels/"
  <> show (channelGuild ch)
  <> "/"
  <> show (channelId ch)

inviteUrl :: Invite -> Text
inviteUrl Invite{inviteCode} = "https://discord.gg/" <> inviteCode

tillJust :: Monad m => m (Maybe a) -> m a
tillJust m = m >>= maybe (tillJust m) pure

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
