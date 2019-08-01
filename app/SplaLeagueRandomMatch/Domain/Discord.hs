{-# LANGUAGE OverloadedStrings #-}
module Dmoain.Discord where

import P hiding (isPrefixOf)
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text as T
import Control.Concurrent (threadDelay)

import Discord
import Discord.Types
import qualified Discord.Requests as R

{-

{
  "id": "80351110224678912",
  "username": "Nelly",
  "discriminator": "1337",
  "avatar": "8342729096ea3675442027381ff50dfe",
  "verified": true,
  "email": "nelly@discordapp.com",
  "flags": 64,
  "premium_type": 1
}

 * 空いている channel 一覧を取得
 * 4人分の user id から channel を作成

-}
-- orhpn...
instance Exception RestCallErrorCode

readDiscordToken :: IO Text
readDiscordToken = do
  tok <- readFileText "discord.token"
  pure $ T.strip tok

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- readDiscordToken
  let opts = def
        { discordToken = tok
        , discordOnEvent = eventHandler
        , discordOnStart = onStart
        }
  userFacingError <- runDiscord opts
  putTextLn userFacingError

onStart :: DiscordHandle -> IO ()
onStart dis = do
  pguilds <- restCall dis $ R.GetCurrentUserGuilds
  print pguilds
  pure ()

listGuildMembersAll :: GuildId -> DiscordHandle -> IO [GuildMember]
listGuildMembersAll gid dis =
  paginateAll
    (Snowflake 0)
    (\uid gm -> max uid (gm & memberUser & userId))
    (\uid -> either throwIO pure =<< restCall dis (R.ListGuildMembers gid (R.GuildMembersTiming (Just 1000) (Just uid))))

-- | pagination を持つ APIから全てのデータを取得する
-- ただし複数回リエクストを行なった場合、途中の増減については考慮されていない。
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

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event =
  case event of
    MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
      _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
      threadDelay (4 * 10^6)
      _ <- restCall dis (R.CreateMessage (messageChannel m) "Pong!")
      pure ()
    _ -> pure ()
  where
    fromBot :: Message -> Bool
    fromBot m = userIsBot (messageAuthor m)

    isPing :: Text -> Bool
    isPing = ("ping" `isPrefixOf`) . toLower
