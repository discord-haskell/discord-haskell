# Discord.hs -- A set of Haskell bindings for Discord

### Theory:
  - Network.Discord.\* contains the type data needed for the library to work
  - Network.Discord contains the transformations on the types that make the library work
  - The general idea of a client is it represents a state machine (clientSM)
  - Dispatch events as defined in the Discord api are represented as on\*Event\* functions in a client instance
  - on\* functions represent transformations in client state
  - As on\* events are triggered by side effects (and will probably result in side effects to do useful things), they are wrapped in an IO monad.

### TODO:
  - [x] User objects
  - [x] Channel objects
  - [x] Message objects (and dependancies)
  - [x] Bot authentication (via token)
  - [ ] Sharding
  - [ ] Websockets
    - [ ] Resuming sessions
    - [ ] Refine Client function signatures
    - [x] Data types for payload parsing
    - [x] State machine abstraction
      - [x] Login
      - [x] Heartbeat
      - [x] Recieve events
        - Note: Can't send messages over websockets
  - [ ] HTTP
    - [x] Login
    - [x] Send messages
    - [ ] Query users
    - [ ] Query guilds
    - [ ] Manage messages (Edit, delete, ect)
    - [ ] Manage Guilds
    - [ ] File uploads
  - [ ] Proper repo branching (Probably targeting around 0.1.0, I don't see a stable happening until then)
  - [ ] Document all of the above shit <- I will probably never be happy with this one

### Roadmap:
  - Push to github
  - 0.1.0
    - "I'm an idiot" pass
    - This is the "I *think* it's actually okay to write a bot with this lib" point
  - 0.5.0:
    - Functional of some sort (Message events, query guilds, users, roles)
    - Be comfortable enough with the general library architecture to attempt to maintain backwards-compat
  - 1.0.0:
    - Feature complete (Supports everything the Discord API does)

### Known problems:
  - I hardcoded the ws gateway. I'm a horrible person who kills small animals for fun, I know, but I can't think of a solution to URI parsing that I like.
  - Client will crash whenever it sees an event it doesn't recognize
  - Client will crash if it sees a payload it doesn't recognize
  - Library has a ton of stuff nobody needs (the EkkoBot example compiles into a 23 MB binary ???!!!)
  - Namespace makes absolutely no sense yet
  - The bot doesn't actually log off when the client closes. I don't actually know how to handle interrupts in haskell yet (Still learning, forgive me pls)
