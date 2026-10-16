
### Intents

#### Privileged Intents


Discord servers are enforcing a rule that bots cannot access Message Content without declaring a Privileged Intent. https://support-dev.discord.com/hc/en-us/articles/4404772028055

Apps with fewer than 10,000 guild-installed users can enable privileged intents in the Developer Portal; apps reaching this threshold require review for continued access. See [Discord's current requirements](https://support-dev.discord.com/hc/en-us/articles/6205754771351-How-do-I-get-Privileged-Intents-for-my-bot).

1. Go https://discord.com/developers/applications

2. Click on the application you want to authorize

3. In the taskbar on the left, select the 'Bot' tab

4. Scroll down to "Privileged Gateway Intents"

5. Enable Presence, Server Members, and Message Content intents.

![image of privileged gateway intents UI](https://user-images.githubusercontent.com/37496339/130155242-581d8ca9-c053-423b-985d-53ce0b88a205.png)
