{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Discord.Internal.Rest.AutoModeration 
  ( AutoModerationRequest(..)
  , MakeAutoModerationRule(..)
  ) where

import Data.Aeson
import Discord.Internal.Types
import Discord.Internal.Rest.Prelude
import           Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R

instance Request (AutoModerationRequest a) where
  majorRoute  = autoModerationMajorRoute
  jsonRequest = autoModerationJsonRequest

data AutoModerationRequest a where
  -- | Returns all audo moderation rules in a guild
  ListAutoModerationRules  :: GuildId -> AutoModerationRequest [AutoModerationRule]
  -- | Returns an auto moderation rule from its id
  GetAutoModerationRule    :: GuildId -> AutoModerationRuleId -> AutoModerationRequest AutoModerationRule
  -- | Creates an auto moderation rule in a guild
  CreateAutoModerationRule :: GuildId -> MakeAutoModerationRule -> AutoModerationRequest ()
  -- | Modifies (Replaces) an auto moderation rule in a guild
  ModifyAutoModerationRule :: GuildId -> AutoModerationRuleId -> MakeAutoModerationRule -> AutoModerationRequest AutoModerationRule
  -- | Deletes an auto moderation rule from its id
  DeleteAutoModerationRule :: GuildId -> AutoModerationRuleId -> AutoModerationRequest ()


data MakeAutoModerationRule = MakeAutoModerationRule
  { makeAutoModerationRuleName            :: String
  , makeAutoModerationRuleEventType       :: AutoModerationRuleEventType
  , makeAutoModerationRuleTriggerType     :: AutoModerationRuleTriggerType
  , makeAutoModerationRuleTriggerMetadata :: Maybe AutoModerationRuleTriggerMetadata
  , makeAutoModerationRuleActions         :: [AutoModerationRuleAction]
  , makeAutoModerationRuleEnabled         :: Bool
  , makeAutoModerationRuleExemptRoles     :: [RoleId]
  , makeAutoModerationRuleExemptChannels  :: [ChannelId]
  } deriving ( Show, Read )

instance FromJSON MakeAutoModerationRule where
  parseJSON = withObject "MakeAutoModerationRule" $ \o ->
    MakeAutoModerationRule <$> o .:  "name"
                           <*> o .:  "event_type"
                           <*> o .:  "trigger_type"
                           <*> o .:? "trigger_metadata"
                           <*> o .:  "actions"
                           <*> o .:  "enabled"
                           <*> o .:  "exempt_roles"
                           <*> o .:  "exempt_channels"

instance ToJSON MakeAutoModerationRule where
  toJSON MakeAutoModerationRule{..} = objectFromMaybes
    [ "name"              .== makeAutoModerationRuleName
    , "event_type"        .== makeAutoModerationRuleEventType
    , "trigger_type"      .== makeAutoModerationRuleTriggerType
    , "trigger_metadata"  .=? makeAutoModerationRuleTriggerMetadata
    , "actions"           .== makeAutoModerationRuleActions
    , "enabled"           .== makeAutoModerationRuleEnabled
    , "exempt_roles"      .== makeAutoModerationRuleExemptRoles
    , "exempt_channels"   .== makeAutoModerationRuleExemptChannels
    ]

autoModerationMajorRoute :: AutoModerationRequest a -> String
autoModerationMajorRoute c = case c of
  (ListAutoModerationRules g) ->          "guild " <> show g
  (GetAutoModerationRule g _) ->          "guild " <> show g
  (CreateAutoModerationRule g _) ->       "guild " <> show g
  (ModifyAutoModerationRule g _ _) ->     "guild " <> show g
  (DeleteAutoModerationRule g _) ->       "guild " <> show g

autoModerationJsonRequest :: AutoModerationRequest r -> JsonRequest
autoModerationJsonRequest c = case c of
  (ListAutoModerationRules guild) ->
      Get (guilds /~ guild /: "auto-moderation" /: "rules") mempty
  
  (GetAutoModerationRule guild amid) ->
      Get (guilds /~ guild /: "auto-moderation" /: "rules" /~ amid) mempty

  (CreateAutoModerationRule guild autoModRule) ->
      let body = pure (R.ReqBodyJson autoModRule)
      in Post (guilds /~ guild /: "auto-moderation" /: "rules") body mempty

  (ModifyAutoModerationRule guild amid patch) ->
      let body = pure (R.ReqBodyJson patch)
      in Patch (guilds /~ guild /: "auto-moderation" /: "rules" /~ amid) body mempty

  (DeleteAutoModerationRule guild amid) ->
      Delete (guilds /~ guild /: "auto-moderation" /: "rules" /~ amid) mempty
  
  where
    guilds :: R.Url 'R.Https
    guilds = baseUrl /: "guilds"
