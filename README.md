# Shell remote control from telegram

## Description of the ShellRemoteBot

Helps you work with your server (or remote computer) via telegram bot emulating terminal operations (sh for Linux and cmd for Windows) and a file manager functionality.

It is possible to work in the terminal from under any user in OS. You can configure access to several telegram users or only for You.

The program can run as a regular console program or run as a daemon/service.

Tested in Linux but You can try to use in other OS also. Developed with FreePascal. It depends on fp-telegram lib .

## Script files (macros)
You can specify predefined shell scripts. Create script file with extention `.script` in the same folder where INI file is located. 
Get the script menu by command `/scripts`. Click the selected script.

## File manager
You can call the file manager by `/dir` command then select directory inline button (select another folder) or file inline button (select the file to download).

You can set the default value in the configuration file (`DefaultDir` in `[File]` section)

## Installation

1. Create bot https://core.telegram.org/bots/#3-how-do-i-create-a-bot . You must select (keep default) longpolling method for getting updates.
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

[File]
;; You can specify default initial directory path for file manager called by /dir command
DefaultDir=
```

3. Run as a console program or daemon/service.

### DEB package
You can easily install/upgrade using a .deb file on Debian/Ubuntu/etc.

#### installation with deb 
Download deb package from release section https://github.com/Al-Muhandis/ShellRemoteBot/releases
Then install for example:
```SH
sudo dpkg --install tgshd_1.2.5-2_amd64.deb
# After package installation set up at least token and admin user id in the INI file located at /etc/tgshd/tgshd.ini
# for example with Nano editor
sudo nano /etc/tgshd/tgshd.ini
# Now You can run /usr/bin/tgshd -r
# Or is better enable and run as a daemon (service file for systemd inlcluded in the DEB package)
systemctl enable tgshd
systemctl start tgshd
```
#### Upgrade with deb

```SH
sudo systemctl stop tgshd
sudo dpkg --install tgshd_1.2.5-2_amd64.deb
sudo systemctl daemon-reload
systemctl start tgshd
```

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
### /dir
Calls the file Manager. You can set the default value in the configuration file
### /delete
With the command (`/delete` `/fullpath/to/file`) You can delete file

## Possible problems

In case of a similar error: `[Error] [HTTP Post JSON] Httpclient: Invalid Protocol`
You may be able to install SSL libraries on the system (for Linux):
```sh
sudo apt installs libssl-dev
```


## HTTP proxy
If you want to add HTTP proxy support, then:
+ add `laz_synapse.lpk` depending on the project
+ add `tgsynapsehttpclientbroker` to the `uses` block
+ set the proxy data in the `INI` file.
Note: The native FPHTTPClient does not yet have full HTTPS proxy support. So in the case of synapse HTTP client, 
to make the HTTPs proxy work in Linux (laz_synapse.lpk v40.1), comment out the line in the blcksock.pas source unit of the synapse library.
`// Port := IntToStr(ResolvePort(Port));`
