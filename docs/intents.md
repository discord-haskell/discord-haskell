
### Intents

#### Privileged Intents


Discord servers are enforcing a rule that bots cannot access Message Content without declaring a Privileged Intent. https://support-dev.discord.com/hc/en-us/articles/4404772028055

Bots in fewer than 75 (ie most bots) will need to check a box in the developer docs. Bots in >75 serves will need to be verified to have access to these intents. 

1. Go https://discord.com/developers/applications

2. Click on the application you want to authorize

3. In the taskbar on the left, select the 'Bot' tab

4. Scroll down to "Privileged Gateway Intents"

5. Enable Presence, Server Members, and Message Content intents.

![image of privileged gateway intents UI](https://user-images.githubusercontent.com/37496339/130155242-581d8ca9-c053-423b-985d-53ce0b88a205.png)
