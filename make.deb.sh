# Linux shell script to create .deb package
# Go to the repository local folder. For example, cd /home/User/ShellRemoteBot
set -eu
STAGING_DIR=./deb.staging
if [ ! -f "${STAGING_DIR}/usr/bin/tgshd" ]; then
    echo "Binary is missing: ${STAGING_DIR}/usr/bin/tgshd (build it first: bash make.sh build)" >&2
    exit 1
fi
find "${STAGING_DIR}" -type d -exec chmod 0755 {} \;
find "${STAGING_DIR}" -type f -exec chmod 0644 {} \;
find "${STAGING_DIR}/usr/bin" -type f -exec chmod 0755 {} \;
find "${STAGING_DIR}/DEBIAN" -maxdepth 1 -type f ! -name conffiles ! -name control -exec chmod 0755 {} \;
sed --in-place '/^Installed-Size:/d' "${STAGING_DIR}/DEBIAN/control"
SIZE_IN_KB="$(du -s ${STAGING_DIR} | awk '{print $1;}')"
echo "Installed-Size: ${SIZE_IN_KB}" >> "${STAGING_DIR}/DEBIAN/control"
[ -f deb.staging/usr/share/doc/tgshd/changelog.Debian.gz ] || gzip -9 -n deb.staging/usr/share/doc/tgshd/changelog.Debian
read -p "Input package filename: " PACKAGE_NAME
dpkg-deb --root-owner-group --build "${STAGING_DIR}" "${PACKAGE_NAME}.deb"
