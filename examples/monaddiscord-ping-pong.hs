{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO.Concurrent

import Discord.Monad
import Discord.Types
import qualified Discord.Requests as R
import Data.Default
import Data.Aeson
import Discord.Internal.Gateway.Cache
import Discord.Internal.Gateway (GatewayException)
import Discord.Internal.Rest.Prelude
import Discord.Internal.Rest
import Control.Monad.State
import Control.Monad.Identity
import Data.Time.Calendar

-- Allows this code to be an executable. See discord-haskell.cabal
main :: IO ()
main = do
  print performPurePingPong
  if testserverid == 0
       then TIO.putStrLn "ERROR: modify the source and set testserverid to your serverid"
       else pingpongExample

-- check the url in a discord server
--                                <server id>           <channel id>
-- https://discord.com/channels/2385235298674262408/4286572469284672046
testserverid :: Snowflake
testserverid = 0

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  err <- runDiscordM id $ (def :: RunDiscordOpts) { discordToken = tok
                          , discordOnStart = startHandler
                          , discordOnEnd = liftIO $ threadDelay (round (0.4 * 10^6)) >>  putStrLn "Ended"
                          , discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                          , discordGatewayIntent = def {gatewayIntentMembers = True, gatewayIntentPrecenses =True}
                          }

  print $ runPurePingPong (def { events = [Right (Right (GuildRoleDelete 0 1))] })

  -- only reached on an unrecoverable error
  -- put normal 'cleanup' code in discordOnEnd
  TIO.putStrLn err

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
--
-- This start handler uses no IO, which means that it's pretty pure.
startHandler :: (MonadDiscord m) => m ()
startHandler = do
  let activity = def { activityName = "monad discord ping-pong"
                     , activityType = ActivityTypeGame
                     }
  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsGame = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

  chans' <- restCall $ R.GetGuildChannels testserverid
  case chans' of
    Right chans -> forM_ (take 1 (filter isTextChannel chans))
                    (\channel -> restCall $ R.CreateMessage (channelId channel)
                                    "Hello! I will reply to pings with pongs")
    Left _ -> return ()


-- If an event handler throws an exception, discord-haskell will continue to run
--
-- This event handler uses no IO, which means it's pretty pure.
eventHandler :: (MonadDiscord m) => Event -> m ()
eventHandler event = case event of
      MessageCreate Message {messageContent = "stop"} -> stopDiscord
      MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")

        -- A very simple message.
        msg <- restCall (R.CreateMessage (messageChannelId m) "Pong")
        case msg of
          Left _ -> return ()
          Right m' -> do
            void $ restCall (R.EditMessage (messageChannelId m, messageId m') (def {R.messageDetailedContent=messageContent m' <> "!"}))

            -- A more complex message. Text-to-speech, does not mention everyone nor
            -- the user, and uses Discord native replies.
            -- Use ":info" in ghci to explore the type
            let opts :: R.MessageDetailedOpts
                opts = def { R.messageDetailedContent = "Here's a more complex message, but doesn't ping @everyone!"
                          , R.messageDetailedTTS = True
                          , R.messageDetailedAllowedMentions = Just $
                              def { R.mentionEveryone = False
                                  , R.mentionRepliedUser = False
                                  }
                          , R.messageDetailedReference = Just $
                              def { referenceMessageId = Just $ messageId m }
                          }
            void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
      _ -> return ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent

-- everything below here is trying to implement an example of a pure discord bot
-- simulation. it's not very good. this is due to the request types being more 
-- opaque than I realised.

data Store = Store
  { cache :: Cache, -- ^ cache to peek into
    events :: [Either T.Text (Either GatewayException Event)], -- ^ events to receive
    restCalls :: [(JsonRequest, String, Either RestCallErrorCode String)], -- ^ what's been sent and then received
    requestResponses :: [(Value, String)], -- ^ what the responses will be, in order
    commands :: [GatewaySendable], -- ^ commands that were sent
    storeLog :: [T.Text] -- ^ stuff that was logged
  }

instance Show Store where
  show (Store c e r rr cs l) = "Store " ++ show c ++ " " ++ show e ++ " " ++ show ((\(_, b, a) -> (b, a)) <$> r) ++ " " ++ show rr ++ " " ++ show cs ++ " " ++ show l

instance Default Store where
  def =
    Store
      { cache = Cache emptyUser mempty mempty mempty (PartialApplication 101 0),
        events = [],
        restCalls = [],
        requestResponses = [],
        commands = [],
        storeLog = []
      }

type PureDiscord = State Store

instance {-# OVERLAPPING #-} MonadDiscord PureDiscord where
  restCall r = do
    rr <- gets requestResponses
    let (v, ret, xs) = case rr of
          [] -> ("ran out of responses", convResult $ fromJSON Null, [])
          ((v', s) : xs') -> (s, convResult $ fromJSON v', xs')
    modify
      ( \s ->
          s
            { restCalls = (jsonRequest r, majorRoute r, v <$ ret) : restCalls s,
              requestResponses = xs
            }
      )
    return ret
    where
      convResult (Success a) = Right a
      convResult (Error e) = Left $ RestCallErrorCode (-1) "parse error" (T.pack e)
  sendCommand gs = modify (\s -> s {commands = gs : commands s})
  readCache = gets cache
  getEvent = do
    s <- gets events
    case s of
      [] -> return (Left "no events")
      (x : xs) -> modify (\s' -> s' {events = xs}) >> return x
  stopDiscord = modify (\s -> s {events = []})

emptyUser :: User
emptyUser = User 0 "" Nothing Nothing False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

emptyMessage :: Message
emptyMessage = Message 0 0 Nothing emptyUser Nothing "" (UTCTime (ModifiedJulianDay 0) 0) Nothing False False [] [] [] [] [] Nothing False Nothing MessageTypeDefault Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- old way to fake, not very effective
-- fakeTheRequest :: (FromJSON a) => Request (r a) => r a -> (Either T.Text a, T.Text)
-- fakeTheRequest r = conv $ case majorRoute r of
--   "msg 0" -> toJSON' (emptyMessage {messageContent = "Hi there"})
--   "guild_chan 0" -> toJSON' [ChannelText 0 0 "channel one" 0 [] 0 False "" Nothing Nothing]
--   t -> (Null, T.pack t)
--   where
--     toJSON' :: (ToJSON a, Show a) => a -> (Value, T.Text)
--     toJSON' a = (toJSON a, T.pack $ show a)
--     conv :: FromJSON a => (Value, T.Text) -> (Either T.Text a, T.Text)
--     conv (Null,t) = (Left ("not implemented:" <> t) , "")
--     conv (v, t) = case fromJSON v of
--       Error e -> (Left (T.pack e), t)
--       Success a -> (Right a, t)

pureRunDiscordOpts :: EnvRunDiscordOpts PureDiscord PureDiscord
pureRunDiscordOpts =
  (def :: EnvRunDiscordOpts PureDiscord PureDiscord)
    { discordOnLog = \t -> modify (\s' -> s' {storeLog = t : storeLog s'}),
      discordOnStart = startHandler,
      discordOnEvent = eventHandler,
      discordOnEnd = modify (\s' -> s' {storeLog = "Ended" : storeLog s'})
    }

runPurePingPong :: Store -> (T.Text, Store)
runPurePingPong s = runIdentity $ flip runStateT s $ runDiscordMPure id pureRunDiscordOpts

performPurePingPong :: (T.Text, Store)
performPurePingPong =
  runPurePingPong store
  where
    mkResponsePair :: (Show a, ToJSON a) => a -> (Value, String)
    mkResponsePair a = (toJSON a, show a)
    store =
      def
        { events = [Right (Right (MessageCreate (emptyMessage {messageContent = "ping"})))],
          requestResponses =
            [ mkResponsePair [ChannelText 0 0 "channel one" 0 [] 0 False "" Nothing Nothing],
              mkResponsePair (emptyMessage {messageContent = "Hello! I will reply to pings with pongs"}),
              mkResponsePair (),
              mkResponsePair (emptyMessage {messageContent = "Pong"}),
              mkResponsePair (emptyMessage {messageContent = "Pong!"}),
              mkResponsePair (emptyMessage {messageContent = "Here's a more complex message, but doesn't ping @everyone!", messageTts = True, messageEveryone = False, messageReferencedMessage = Just emptyMessage {messageContent = "you got me, this is fake"}})
            ]
        }
