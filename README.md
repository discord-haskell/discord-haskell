# Discord.hs

## REWORK TIME!!!
---

A majority of d.hs was hastily thrown together, and was honestly intended as a proof of
concept. Now that the code base is in a semi-stable state, and there is a product that I
can fall back on, it's time to go through and make everything properly polymorphic and
modular.

## Roadmap
---

Discord-hs will consist of ~4 modules

- discord-types: Currently Discord.Types. Splitting this off into another package will
  allow code components to be written for Discord without actually importing code for
  running bots.

- discord-rest: Currently Discord.Rest. Splitting this off will allow code to be written
  that interfaces with the Discord api that doesn't actually need a gateway.

- discord-core: Currently (mostly) Discord.Gateway. Gateway code needs to exist somewhere.
  This code will be made polymorphic over a new typeclass defined in discord-types.

- discord-hs: Currently Discord.Framework (and others). Framework code allowing discord bots
  to be created. Will use a typesafe combinator system inspired by Servant. Will additionally
  export all of the above code.
