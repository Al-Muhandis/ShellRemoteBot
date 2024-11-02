#!/usr/bin/env bash

function priv_clippit
(
    cat <<EOF
Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

function priv_dpkg
(
    # Linux shell script to create .deb package
    # Go to the repository local folder. For example, cd /home/User/ShellRemoteBot
    declare -r STAGING_DIR=./deb.staging
    find "${STAGING_DIR}" -type d -exec chmod 0755 {} +
    find "${STAGING_DIR}" -type f -exec chmod 0644 {} +
    find "${STAGING_DIR}/usr/bin" -type f -exec chmod 0755 {} +
    SIZE_IN_KB="$(du -s ${STAGING_DIR} | awk '{print $1;}')"
    echo "Installed-Size: ${SIZE_IN_KB}" >> "${STAGING_DIR}/DEBIAN/control"
    gzip -9 -n deb.staging/usr/share/doc/tgshd/changelog.Debian
    read -p "Input package filename: "
    dpkg-deb --root-owner-group --build "${STAGING_DIR}" "${REPLY}.deb"
)

function priv_pkgsearch
(
    if ((${#})); then
        for REPLY in "${@}"; do
            lazbuild --verbose-pkgsearch "${REPLY}" || lazbuild --add-package "${REPLY}"
        done
    fi
)

function priv_packages
(
    if [[ -d 'use' ]]; then
        git submodule update --init --recursive
        git submodule update --recursive --remote
    else
        mkdir 'use'
    fi
    if ((${#})); then
        for REPLY in "${@}"; do
            declare -A VAR=(
                [url]="https://packages.lazarus-ide.org/${REPLY}.zip"
                [out]=$(mktemp)
            )
            wget --output-document "${VAR[out]}" "${VAR[url]}"
            unzip -o "${VAR[out]}" -d "use/${REPLY}"
            rm --verbose "${VAR[out]}"
        done
    fi
    find 'use' -type 'f' -name '*.lpk' -exec lazbuild --add-package-link {} +
)

function priv_main
(
    set -euo pipefail
    if ! (which lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus
            ;;
        esac
    fi
    if ((${#})); then
        case ${1} in
            build)
                priv_pkgsearch
                priv_packages
                find 'src' -type 'f' -name '*.lpi' \
                    -exec lazbuild --no-write-project --recursive --no-write-project --build-mode=release {} 1>&2 +
                ;;
            dpkg) priv_dpkg ;;
        esac
    else
        priv_clippit
    fi
)

priv_main "${@}" >/dev/null
