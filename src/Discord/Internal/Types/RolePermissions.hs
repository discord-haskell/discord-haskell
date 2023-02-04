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

import Data.Bits (Bits (complement, shift, (.&.), (.|.)))
import Discord.Internal.Types.Guild
  ( Guild,
    Role (rolePerms),
    roleIdToRole,
  )
import Discord.Internal.Types.Prelude (RolePermissions)
import Discord.Internal.Types.User (GuildMember (memberRoles))

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
  deriving (Eq, Ord, Enum, Show)

permissionBits :: PermissionFlag -> RolePermissions
permissionBits p = shift 1 (fromEnum p)

-- | Check if a given role has all the permissions
hasRolePermissions :: [PermissionFlag] -> RolePermissions -> Bool
hasRolePermissions permissions rolePermissions = (.&.) combinedPermissions rolePermissions == combinedPermissions
  where
    combinedPermissions = combinePermissions permissions

-- | Check if a given role has the permission
hasRolePermission :: PermissionFlag -> RolePermissions -> Bool
hasRolePermission p r = (.&.) (permissionBits p) r > 0

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
combinePermissions = foldr ((.|.) . permissionBits) 0

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
