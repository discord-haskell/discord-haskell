-- | Re-export user-visible types
module Discord.Types
  ( module Discord.Internal.Types
  ) where

import Discord.Internal.Types hiding
    ( GatewaySendableInternal(..)
    , GatewayReceivable(..)
    , EventInternalParse(..)
    , InternalDiscordType(..)

    , colorToInternal
    , convertToRGB
    , hexToRGB
    )
