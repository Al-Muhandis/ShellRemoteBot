#!/usr/bin/env bash
##############################################################################################################

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
    echo "Installed-Size: $(du -s ${STAGING_DIR} | awk '{print $1;}')" >>"${STAGING_DIR}/DEBIAN/control"
    gzip -9 -n 'deb.staging/usr/share/doc/tgshd/changelog.Debian'
    read -rp "Input package filename: "
    dpkg-deb --root-owner-group --build "${STAGING_DIR}" "${REPLY}.deb"
)

function priv_main
(
    set -euo pipefail
    if ((${#})); then
        case ${1} in
            build)
                if ! (which 'lazbuild'); then
                    source '/etc/os-release'
                    case ${ID:?} in
                        debian | ubuntu)
                            sudo apt-get update
                            sudo apt-get install -y lazarus
                            ;;
                    esac
                fi
                if [[ -d 'use' ]]; then
                    git submodule update --recursive --init
                    git submodule update --recursive --remote
                    find 'use' -type 'f' -name '*.lpk' -exec lazbuild --add-package-link {} +
                fi
                find 'src' -type 'f' -name '*.lpi' -exec \
                    lazbuild --no-write-project --recursive --no-write-project --build-mode=release {} + 1>&2
                ;;
            dpkg) priv_dpkg ;;
        esac
    else
        priv_clippit
    fi
)

##############################################################################################################
priv_main "${@}" >/dev/null
