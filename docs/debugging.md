
### Debugging


```haskell
example :: IO ()
example = do userFacingError <- runDiscord $ def
                 { discordToken = "Bot ZZZZZZZZZZZZZZZZZZZ"

            --     discordOnLog :: T.Text -> IO ()
                 , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                 }

             -- userFacingError :: T.Text
             TIO.putStrLn userFacingError

```


1. Always print the `userFacingError` Text returned from `runDiscord`. This is used for errors that cannot be recovered from.

2. Use the `discordOnLog` handler to print debugging information as it happens.


If something else goes wrong with the library please open an issue. It is helpful,
but not always necessary, to attach a log.

Assign a handler to the `discordOnLog :: Text -> IO ()` to print info as it happens.
Remember to remove sensitive information before posting.
