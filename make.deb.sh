#!/usr/bin/env bash

# Linux shell script to create .deb package.
# Thin wrapper around "bash make.sh dpkg" so the .deb build logic (staging
# chmod 0755 for maintainer scripts, Installed-Size dedupe, gzip guard,
# binary check) lives in a single place and cannot drift apart.
# Go to the repository local folder. For example, cd /home/User/ShellRemoteBot
#     bash make.deb.sh [package-filename]
# With no argument the package filename is requested interactively; the
# argument form is meant for non-interactive CI.
set -eu
exec bash make.sh dpkg "$@"
