
### Embeds

Embeds are special messages with boarders and images. [Example embed created by discord-haskell](./examples/embed-photo.jpg)

The `Embed` record (and sub-records) store embed data received from Discord.

The `CreateEmbed` record stores data when we want to create an embed.

`CreateEmbed` has a `Default` instance, so you only need to specify the fields you use:

```haskell
_ <- restCall (R.CreateMessageEmbed <channel_id> "Pong!" $
        def { createEmbedTitle = "Pong Embed"
            , createEmbedImage = Just $ CreateEmbedImageUpload <bytestring>
            , createEmbedThumbnail = Just $ CreateEmbedImageUrl
                    "https://avatars2.githubusercontent.com/u/37496339"
            })
```

Uploading a file each time is slow, prefer uploading images to a hosting site like imgur.com, and then referencing them.
