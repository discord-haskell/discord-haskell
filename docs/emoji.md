
### Emoji

For single character Emoji you can use the unicode name ("eyes", "fire", etc).

For multi-character Emoji you must use the discord format. Type `\:emoji:` into
a discord chat and paste that into the Text

For example `:thumbsup::skin-tone-3:` is `"👍\127997"`.
A custom emoji will look like `<name:id_number>` or `name:id_number`.

See [examples/ping-pong.hs](https://github.com/discord-haskell/discord-haskell/blob/master/examples/ping-pong.hs)
 for a `CreateReaction` request in use.
