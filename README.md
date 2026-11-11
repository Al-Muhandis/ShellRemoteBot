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
You can call the file manager by `/dir` command then select directory inline button (select another folder) or file inline button.

Clicking a file does not download it right away: the listing is replaced (edited) with a **file card** showing the file name, its human-readable size (B/K/M/G/T), the modification time and, on Unix, also the rights (octal mode) and the numeric owner (`uid:gid`). The card offers four actions:
- `⬇ Download` — sends the file as a document. Files larger than 50 MB (the Bot API limit) are refused with a hint to split or zip them via the shell instead, nothing is uploaded;
- `✏ Edit` — opens the file for inline editing (files up to 1 MB, plain text/UTF-8 only). The current content is shown; reply with the new text to overwrite it. A `.bak` copy is kept automatically — if anything goes wrong, the `↩ Revert` button on the confirmation screen restores the original;
- `🗑 Delete` — the same mandatory Ok/Cancel confirmation as `/delete`; after a successful delete the bot reopens the parent folder listing instead of a separate "deleted" note;
- `◀ Back` — reopens the parent folder listing on the same page the card was opened from.

You can set the default value in the configuration file (`DefaultDir` in `[File]` section)

Big folders are split into pages navigated with the ◀️ / ▶️ buttons (90 entries per page); the ◀️ button shows the current position among the pages, e.g. `◀️ 2/5`. File buttons carry the human-readable size (`report.pdf (1.5 M)`); long names are trimmed with `…` so the buttons stay narrow.

The `+ Folder` button creates a folder inside the current one: the bot asks for the name with a reply prompt — send the name as text, or `/cancel`. The name must be a single path element (no `/` or `\`, not `.` or `..`); a wrong name or an already existing folder is refused with the reason, and after a successful creation the listing is redrawn.

The `Upload...` button marks the folder as the upload destination: send a document as a reply to that bot message and it is saved into the folder.

The inline buttons keep temporary references to the listed paths, they do not survive a bot restart: after an upgrade an old listing answers `Listing is outdated. Call /dir again` and you simply call `/dir` again.

### Rich /dir listing (optional)
Set `RichMessages=true` in the `[API]` section and a fresh `/dir` listing is sent as a rich message (Bot API 10.1+): the same markdown header with the listing keyboard mirrored into rich buttons. If the rich send fails (an old Bot API endpoint, a rejected payload), the bot automatically falls back to the classic `sendMessage` with the very same markup and logs the reason. Page updates (◀️ / ▶️) stay classic message edits, so listings keep working on old Telegram clients regardless of this setting.

## Installation

1. Create bot https://core.telegram.org/bots/#3-how-do-i-create-a-bot . You must select (keep default) longpolling method for getting updates.
2. Create config ini file `tgshd.ini` in app folder:

``` INI
[API]
;; Specify Your token
Token=
;; Actual if telegram server is blocked in server/computer region
Endpoint=https://api.telegram.org/bot
;; Timeout for longpolling requests while getting updates
Timeout=20
;; true: the /dir listing is sent as a rich message (Bot API 10.1+).
;; Rich messages render properly only on fresh Telegram clients, so this
;; is opt-in (default false) and every failed rich send falls back to the
;; classic listing automatically
RichMessages=false

[Users]
;; Specify all admin users (Telegram UserID [Integer]). For example:
;;123456789=a
;; where 'a' character is means administrator, 'b' - banned
;; true = never reply to non-admin users (hides the very existence of the
;; bot), false = answer them "You cannot access to this bot!"
SilentForNonAdmins=false
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
sudo dpkg --install tgshd_1.2.5.5-1_amd64.deb
# After package installation set up at least token and admin user id in the INI file located at /etc/tgshd/tgshd.ini
# for example with Nano editor
sudo nano /etc/tgshd/tgshd.ini
sudo systemctl restart tgshd
# The systemd unit is included in the DEB package, enabled and started automatically.
# On restart of the service it runs /usr/bin/tgshd -r
```
#### Upgrade with deb

```SH
sudo dpkg --install tgshd_1.2.5.5-1_amd64.deb
# The package maintainer scripts stop the old process and restart the service with the new binary automatically.
```

### systemd unit
The deb package ships the `tgshd.service` unit (`/lib/systemd/system/tgshd.service`) and the maintainer scripts enable and start it automatically on install, restart it on upgrade and stop it on removal.

The unit:
- `Type=simple`, `ExecStart=/usr/bin/tgshd -r`, `Restart=on-failure` with a 5 second delay, `WantedBy=multi-user.target`, starts after `network-online.target`
- runs as root and is intentionally NOT sandboxed (no `User=`, no `ProtectSystem=`, no `NoNewPrivileges=`): the bot is a remote root terminal and must be able to edit /etc, use sudo/su and manage services. Restrict access with the `[Users]` section of `/etc/tgshd/tgshd.ini` instead, not with systemd.

Managing it manually:
```SH
sudo systemctl status tgshd
sudo systemctl restart tgshd
# the daemon logs to syslog, journald picks it up:
journalctl -u tgshd
```

## Users rights and telegram user ID
You can define user access in configuration file `tgshd.ini` by adding string like `user_id=a` where user_id is telegram user id of administrator.
You can add more than one administrator.
If any _simple_ telegram user sends any message, command etc to the bot then the bot will reply `You cannot access to this bot!`.
if You add a line `user_id=b` then user will banned and the bot will not respond anything

Set `SilentForNonAdmins=true` in the `[Users]` section to not reply to non-admin users at all: the bot behaves as if it does not exist for them (banned users are always ignored).

There are some ways to determine your user_id but You can simply send `/myid` to [@HelloLazarusBot](https://t.me/hellolazarusbot) chat

## Telegram bot commands
Any text message that is not a command is written to the shell stdin as is, and the shell output is sent back (long output is split into several UTF-8 safe messages).

Messages styled by the fresh Telegram clients arrive as rich messages (Bot API 10.1+) with no plain text field. The bot reads their plain text content instead, so shell input and text replies (e.g. the answer to an edit prompt) keep working. The text of the confirmation buttons and the callback flows is unaffected: it is read from the callback data, not from the message.

On start the bot registers its commands in the Telegram command menu (`setMyCommands`): the menu button of the clients shows `/dir`, `/scripts`, `/read` and `/delete` with short descriptions, and `/sig` as well on Unix. A failed registration is only logged and never prevents the bot from starting.
### /read
Reads messages from the shell console.
Sometimes the program does not wait for a response immediately after execution in the console, and in this case, you can read the newly appeared messages by the `/read` command.
### Control panel for silent commands
When a command is sent to the shell and prints no output right away, the bot posts a small control panel instead of leaving you guessing: `⏳ Command is running…` with the inline buttons
- `⏹ SIGINT` and `✖ SIGKILL` (Unix only — the same signals as `/sigint` and `/sigkill`);
- `📖 Read` — the same as `/read`: the fresh output is edited into the panel message when it fits, otherwise it is sent in parts as usual.

A panel is retired (`✔ Done`, buttons removed) as soon as its command is known to be over: output arrives at the next interaction, a signal from the panel terminates the shell, or a new silent command supersedes the panel — at most one live panel stays in the chat. Like all inline buttons of the bot, the panel buttons keep temporary references that do not survive a bot restart: an old panel answers `The control panel is outdated…`.
### /sig and /sigXXXX
It is worked under Unix systems. The signal is delivered by kill(2) to the whole process group of the shell, i.e. to the shell itself and to the command it is currently running.
For example, `/sig 9` where 9 is kill sig number or You can send commands `/sigint`, `/sigkill`, `/sigquit` and `/sigterm`.
The same SIGINT and SIGKILL are available as the buttons of the control panel of a long-running command (see above).
The shell is started in its own session (setsid), so the signals never reach the bot itself.
If the signal terminates the shell, it is restarted automatically with a fresh environment (the shell state like its working directory and variables is not preserved).
### /scripts
Calls the menu for executing the prepared script from the list of files. Scripts must located in the same folder where the INI file is located,
or you can set the folder in the settings in the the `[Scripts]` section / `Directory`
### /dir
Calls the file Manager. You can set the default value in the configuration file
### /delete
With the command (`/delete` `/fullpath/to/file`) You can delete file. The bot checks the file exists and asks for confirmation with the Ok/Cancel inline buttons; the file is deleted only after Ok, and the bot then shows the parent folder listing instead of a separate "deleted" note. The confirmation expires together with the temporary path references (see File manager).

## Possible problems

In case of a similar error: `[Error] [HTTP Post JSON] Httpclient: Invalid Protocol`
You may be able to install SSL libraries on the system (for Linux):
```sh
sudo apt install libssl-dev
```


## HTTP proxy
If you want to add HTTP proxy support, then:
+ add `laz_synapse.lpk` depending on the project
+ add `tgsynapsehttpclientbroker` to the `uses` block
+ set the proxy data in the `INI` file.
Note: The native FPHTTPClient does not yet have full HTTPS proxy support. So in the case of synapse HTTP client, 
to make the HTTPs proxy work in Linux (laz_synapse.lpk v40.1), comment out the line in the blcksock.pas source unit of the synapse library.
`// Port := IntToStr(ResolvePort(Port));`
