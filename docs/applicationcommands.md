### Application Commands

https://discord.com/developers/docs/interactions/application-commands

There are three steps to application commands.

1. Register the command with discord. This only needs to be done once, but there is no harm in doing it again/on each reboot, as the creation of commands just overwrites other commands of the same type and name.
   1. This is achieved by sending a `CreateGuildApplicationCommand` event to discord. The easiest way to do this is by capturing and working off of the `Ready` event in the event loop, which is only sent in the initial startup.
   2. Commands can either be global (in which case there is a delay to their addition) or they can be guild (server) specific.
   3. There are other application command requests that can be conducted in this step, so please have a look at the documentation.
   4. Finally, the application id (which is needed to create commands) of the bot is stored in the cache if you need to create, edit, delete, or get application commands later in the program.
2. Receive an interaction
   1. This is achieved by capturing the `Interaction` event in the event loop
3. Respond to the interaction
   1. When you have captured an interaction, you can act on the information within, and send a reply using the appropriate method, creating an `InteractionResponse` and sending that with `CreateInteractionResponse`.

Now you're ready to create application commands! If you have any questions, please investigate the source and docs first, and then join the discord server and ask questions there.

