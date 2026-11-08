#!/usr/bin/env bash

function priv_clippit
(
    cat <<EOF
Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
    test    Build and run the FPCUnit test suite
EOF
)

function priv_setup
(
    # Make sure lazbuild is available and the vendored component packages
    # (use/*.lpk) are registered, so any project .lpi can be built
    if ! (which lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus
                ;;
        esac
    fi
    if [[ -f 'use/components.txt' ]]; then
        git submodule update --init --recursive
        while read -r; do
            if [[ -n "${REPLY}" ]] &&
                ! (lazbuild --verbose-pkgsearch "${REPLY}") &&
                ! (lazbuild --add-package "${REPLY}") &&
                ! [[ -f "use/${REPLY}" ]]; then
                    declare -A VAR=(
                        [url]="https://packages.lazarus-ide.org/${REPLY}.zip"
                        [out]=$(mktemp)
                    )
                    wget --output-document "${VAR[out]}" "${VAR[url]}" >/dev/null
                    unzip -o "${VAR[out]}" -d "use/${REPLY}"
                    rm --verbose "${VAR[out]}"
                fi
        done < 'use/components.txt'
        find 'use' -type 'f' -name '*.lpk' -exec lazbuild --add-package-link {} + 1>&2
    fi
)

function priv_lazbuild
(
    # Refresh VersionInfo (src/tgshd.lpi) and the deb control Version from
    # the git tag/describe (see make.ver.sh) before compiling
    bash make.ver.sh 1>&2
    priv_setup
    find 'src' -type 'f' -name '*.lpi' \
        -exec lazbuild --no-write-project --recursive --no-write-project --build-mode=release {} + 1>&2
)

function priv_test
(
    # Build the FPCUnit console suite (tests/tgshdtests.lpi) and run it:
    # the runner exits non-zero when any test fails or errors
    priv_setup
    lazbuild --no-write-project tests/tgshdtests.lpi 1>&2
    tests/tgshdtests --all --format=plain 1>&2
)

function priv_dpkg
(
    # Linux shell script to create .deb package
    # Go to the repository local folder. For example, cd /home/User/ShellRemoteBot
    # Package filename is taken from ${1}; without it it is requested
    # interactively (the argument form is meant for non-interactive CI).
    declare -r STAGING_DIR=./deb.staging
    if [[ -f 'src/x86_64-linux/tgshd' ]]; then
        cp --force 'src/x86_64-linux/tgshd' "${STAGING_DIR}/usr/bin/tgshd"
        echo "Staged binary: src/x86_64-linux/tgshd -> ${STAGING_DIR}/usr/bin/tgshd" 1>&2
    elif [[ ! -f "${STAGING_DIR}/usr/bin/tgshd" ]]; then
        echo "Binary is missing: src/x86_64-linux/tgshd and ${STAGING_DIR}/usr/bin/tgshd (build it first: bash ${0} build)" >&2
        exit 1
    fi
    # Refresh the deb control Version from the git tag/describe (see make.ver.sh)
    bash make.ver.sh 1>&2
    find "${STAGING_DIR}" -type d -exec chmod 0755 {} +
    find "${STAGING_DIR}" -type f -exec chmod 0644 {} +
    find "${STAGING_DIR}/usr/bin" -type f -exec chmod 0755 {} +
    find "${STAGING_DIR}/DEBIAN" -maxdepth 1 -type f ! -name conffiles ! -name control -exec chmod 0755 {} +
    sed --in-place '/^Installed-Size:/d' "${STAGING_DIR}/DEBIAN/control"
    echo "Installed-Size: $(du -s ${STAGING_DIR} | awk '{print $1;}')" >>"${STAGING_DIR}/DEBIAN/control"
    if [[ ! -f 'deb.staging/usr/share/doc/tgshd/changelog.Debian.gz' ]]; then
        gzip -9 -n 'deb.staging/usr/share/doc/tgshd/changelog.Debian'
    fi
    declare PACKAGE_NAME="${1:-}"
    if [[ -z "${PACKAGE_NAME}" ]]; then
        read -rp "Input package filename: " PACKAGE_NAME ||
            { echo "Package filename required for non-interactive runs: bash ${0} dpkg <filename>" >&2; exit 1; }
    fi
    [[ "${PACKAGE_NAME}" == *.deb ]] || PACKAGE_NAME+='.deb'
    dpkg-deb --root-owner-group --build "${STAGING_DIR}" "${PACKAGE_NAME}" 1>&2
)

function priv_main
(
    set -euo pipefail
    if ((${#})); then
        case ${1} in
            build) priv_lazbuild ;;
            test) priv_test ;;
            dpkg) priv_dpkg "${@:2}" ;;
            *) priv_clippit ;;
        esac
    else
        priv_clippit
    fi
)

priv_main "${@}" >/dev/null
