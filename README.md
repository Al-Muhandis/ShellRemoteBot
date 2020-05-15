# Shell remote control from telegram

## Description of the ShellRemoteBot

Helps you work with your server (or remote computer) via telegram bot emulating terminal operations (sh for Linux and cmd for Windows).

It is possible to work in the terminal from under any user in OS. You can configure access to several telegram users or only for You.

The program can run as a regular console program or run as a daemon/service.

Tested in Linux but You can try to use in other OS also. Developed with FreePascal. It depends on fp-telegram lib .

## Installation

1. Create bot https://core.telegram.org/bots/#3-how-do-i-create-a-bot . You must select longpolling method for getting updates.
2. Create config ini file `tgshd.ini` in app folder:

``` INI
[API]
;; Specify Your token
Token=
;; Actual if telegram server is blocked in server/computer region
Endpoint=https://api.telegram.org/bot
;; Timeout for lognpolling requests while getting updates
Timeout=20

[Users]
;; Specify all admin users (Telegram UserID [Integer]). For example: 
;;123456789=a
;; where 'a' character is means administrator, 'b' - banned
YOUR_User_ID=a
[Proxy]
;; If You can want to use HTTP proxy please use tgsynapsehttpclientbroker
Host=
Port=
Username=
Password=

[Scripts]
;; You can specify directory for script file folder. Default value equal configuration file directory
Directory=
```

3. Run as a console program or daemon/service.

## Users rights and telegram user ID
You can define user access in configuration file `tgshd.ini` by adding string like `user_id=a` where user_id is telegram user id of administrator.
You can add more than one administrator.
If any _simple_ telegram user sends any message, command etc to the bot then the bot will reply `You cannot access to this bot!`.
if You add a line `user_id=b` then user will banned and the bot will not respond anything

There are some ways to determine your user_id but You can simply send `/myid` to [@HelloLazarusBot](https://t.me/hellolazarusbot) chat

## Telegram bot commands
### /read
Reads messages from the shell console.
Sometimes the program does not wait for a response immediately after execution in the console, and in this case, you can read the newly appeared messages by the `/read` command.
### /sig and /sigXXXX
It is worked under Unix systems. You can send POSIX signal to terminal. 
For example, `/sig 9` where 9 is kill sig number or You can send commands `/sigint`, `/sigkill`, `/sigquit` and `/sigterm`.
### /scripts
Calls the menu for executing the prepared script from the list of files. Scripts must located in the same folder where the INI file is located,
or you can set the folder in the settings in the the `[Scripts]` section / `Directory`

## HTTP proxy
If you want to add HTTP proxy support, then:
+ add `laz_synapse.lpk` depending on the project
+ add `tgsynapsehttpclientbroker` to the `uses` block
+ set the proxy data in the `INI` file.
Note: The native FPHTTPClient does not yet have full HTTPS proxy support. So in the case of synapse HTTP client, 
to make the HTTPs proxy work in Linux (laz_synapse.lpk v40.1), comment out the line in the blcksock.pas source unit of the synapse library.
`// Port := IntToStr(ResolvePort(Port));`