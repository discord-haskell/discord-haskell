module Discord.Internal.Types.RolePermissions
  ( PermissionFlag (..),
    hasRolePermissions,
    hasRolePermission,
    newRolePermissions,
    newRolePermission,
    setRolePermissions,
    setRolePermission,
    clearRolePermissions,
    clearRolePermission,
    hasGuildMemberPermission,
  )
where

import Prelude hiding (Foldable(..))
import Data.Bits (Bits (complement, shift, (.&.), (.|.)))
import Discord.Internal.Types.Guild
  ( Guild,
    Role (rolePerms),
    roleIdToRole,
  )
import Discord.Internal.Types.Prelude (RolePermissions (..))
import Discord.Internal.Types.User (GuildMember (memberRoles))
import Data.Foldable (foldl')

data PermissionFlag
  = CREATE_INSTANT_INVITE
  | KICK_MEMBERS
  | BAN_MEMBERS
  | ADMINISTRATOR
  | MANAGE_CHANNELS
  | MANAGE_GUILD
  | ADD_REACTIONS
  | VIEW_AUDIT_LOG
  | PRIORITY_SPEAKER
  | STREAM
  | VIEW_CHANNEL
  | SEND_MESSAGES
  | SEND_TTS_MESSAGES
  | MANAGE_MESSAGES
  | EMBED_LINKS
  | ATTACH_FILES
  | READ_MESSAGE_HISTORY
  | MENTION_EVERYONE
  | USE_EXTERNAL_EMOJIS
  | VIEW_GUILD_INSIGHT
  | CONNECT
  | SPEAK
  | MUTE_MEMBERS
  | DEAFEN_MEMBERS
  | MOVE_MEMBERS
  | USE_VAD
  | CHANGE_NICKNAME
  | MANAGE_NICKNAMES
  | MANAGE_ROLES
  | MANAGE_WEBHOOKS
  | MANAGE_EMOJIS_AND_STICKERS
  | USE_APPLICATION_COMMANDS
  | REQUEST_TO_SPEAK
  | MANAGE_EVENTS
  | MANAGE_THREADS
  | CREATE_PUBLIC_THREADS
  | CREATE_PRIVATE_THREADS
  | USE_EXTERNAL_STICKERS
  | SEND_MESSAGES_IN_THREADS
  | USE_EMBEDDED_ACTIVITIES
  | MODERATE_MEMBERS
  | VIEW_CREATOR_MONETIZATION_ANALYTICS
  | USE_SOUNDBOARD
  | CREATE_GUILD_EXPRESSIONS
  | CREATE_EVENTS
  | USE_EXTERNAL_SOUNDS
  | SEND_VOICE_MESSAGES
  | SET_VOICE_CHANNEL_STATUS
  | SEND_POLLS
  | USE_EXTERNAL_APPS
  | PIN_MESSAGES
  | BYPASS_SLOWMODE
  deriving (Eq, Ord, Enum, Show)

permissionBits :: PermissionFlag -> RolePermissions
permissionBits p = shift (RolePermissions 1) (permissionBit p)

-- Each flag's bit as Discord's permissions table gives it. The table has no bit 47.
permissionBit :: PermissionFlag -> Int
permissionBit p = case p of
  CREATE_INSTANT_INVITE               -> 0
  KICK_MEMBERS                        -> 1
  BAN_MEMBERS                         -> 2
  ADMINISTRATOR                       -> 3
  MANAGE_CHANNELS                     -> 4
  MANAGE_GUILD                        -> 5
  ADD_REACTIONS                       -> 6
  VIEW_AUDIT_LOG                      -> 7
  PRIORITY_SPEAKER                    -> 8
  STREAM                              -> 9
  VIEW_CHANNEL                        -> 10
  SEND_MESSAGES                       -> 11
  SEND_TTS_MESSAGES                   -> 12
  MANAGE_MESSAGES                     -> 13
  EMBED_LINKS                         -> 14
  ATTACH_FILES                        -> 15
  READ_MESSAGE_HISTORY                -> 16
  MENTION_EVERYONE                    -> 17
  USE_EXTERNAL_EMOJIS                 -> 18
  VIEW_GUILD_INSIGHT                  -> 19
  CONNECT                             -> 20
  SPEAK                               -> 21
  MUTE_MEMBERS                        -> 22
  DEAFEN_MEMBERS                      -> 23
  MOVE_MEMBERS                        -> 24
  USE_VAD                             -> 25
  CHANGE_NICKNAME                     -> 26
  MANAGE_NICKNAMES                    -> 27
  MANAGE_ROLES                        -> 28
  MANAGE_WEBHOOKS                     -> 29
  MANAGE_EMOJIS_AND_STICKERS          -> 30
  USE_APPLICATION_COMMANDS            -> 31
  REQUEST_TO_SPEAK                    -> 32
  MANAGE_EVENTS                       -> 33
  MANAGE_THREADS                      -> 34
  CREATE_PUBLIC_THREADS               -> 35
  CREATE_PRIVATE_THREADS              -> 36
  USE_EXTERNAL_STICKERS               -> 37
  SEND_MESSAGES_IN_THREADS            -> 38
  USE_EMBEDDED_ACTIVITIES             -> 39
  MODERATE_MEMBERS                    -> 40
  VIEW_CREATOR_MONETIZATION_ANALYTICS -> 41
  USE_SOUNDBOARD                      -> 42
  CREATE_GUILD_EXPRESSIONS            -> 43
  CREATE_EVENTS                       -> 44
  USE_EXTERNAL_SOUNDS                 -> 45
  SEND_VOICE_MESSAGES                 -> 46
  SET_VOICE_CHANNEL_STATUS            -> 48
  SEND_POLLS                          -> 49
  USE_EXTERNAL_APPS                   -> 50
  PIN_MESSAGES                        -> 51
  BYPASS_SLOWMODE                     -> 52

-- | Check if a given role has all the permissions
hasRolePermissions :: [PermissionFlag] -> RolePermissions -> Bool
hasRolePermissions permissions rolePermissions = (.&.) combinedPermissions rolePermissions == combinedPermissions
  where
    combinedPermissions = combinePermissions permissions

-- | Check if a given role has the permission
hasRolePermission :: PermissionFlag -> RolePermissions -> Bool
hasRolePermission p r = getRolePermissions (permissionBits p .&. r) > 0

-- | Replace a users rolePerms
--   with a complete new set of permissions
newRolePermissions :: [PermissionFlag] -> RolePermissions
newRolePermissions = combinePermissions

-- | Get the RolePermissions of a single PermissionFlag
newRolePermission :: PermissionFlag -> RolePermissions
newRolePermission = permissionBits

-- | Update RolePermissions with new permissions
setRolePermissions :: [PermissionFlag] -> RolePermissions -> RolePermissions
setRolePermissions p r = combinePermissions p .|. r

-- | Unset Permissions from RolePermissions
clearRolePermissions :: [PermissionFlag] -> RolePermissions -> RolePermissions
clearRolePermissions p r = (complement . combinePermissions) p .&. r

-- | Set a certain permission flag
--   This method doesn't lose the other already present permissions
setRolePermission :: PermissionFlag -> RolePermissions -> RolePermissions
setRolePermission p = (.|.) (permissionBits p)

-- | Remove a permission from a user by clearing the bit
clearRolePermission :: PermissionFlag -> RolePermissions -> RolePermissions
clearRolePermission p = (.&.) (complement . permissionBits $ p)

combinePermissions :: [PermissionFlag] -> RolePermissions
combinePermissions = foldl' (\rp -> (rp .|.) . permissionBits) (RolePermissions 0)

-- | Check if any Role of an GuildMember has the needed permission
--   If the result of roleIdToRole is Nothing, it prepends a "False"
--   Otherwise it checks for the needed permission
hasGuildMemberPermission :: Guild -> GuildMember -> PermissionFlag -> Bool
hasGuildMemberPermission g gm p = go (memberRoles gm)
  where
    go [] = False
    go (x : xs) = case roleIdToRole g x of
      Nothing -> go xs
      Just a -> p `hasRolePermission` rolePerms a || go xs
