# ShellRemoteBot
Shell remote control from telegram

ShellRemoteBot 

Helps you work with your server (or remote computer) via telegram bot emulating terminal operations (sh for Linux and cmd for Windows).

It is possible to work in the terminal from under any user in OS. You can configure access to several telegram users or only for You.

The program can run as a regular console program or run as a daemon/service.

Tested in Linux but You can try to use in other OS also. Developed with FreePascal. It depends on fp-telegram lib .

1. Create bot https://core.telegram.org/bots/#3-how-do-i-create-a-bot . You must select longpolling method for getting updates.
2. Create config ini file `telegram.ini` in app folder:

``` INI
[Bot]
;; Specify Your token
Token=

[API]
;; Actual if telegram server is blocked in server/computer region
Endpoint=https://api.telegram.org/bot
;; Timeout for lognpolling requests while getting updates
Timeout=20

[Users]
;; Specify all admin users (Telegram UserID [Integer]). For example: 
;;123456789=a
YOUR_User_ID=a
```

3. Run as a console program or daemon/service.