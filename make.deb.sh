# Linux shell script to create .deb package
# Go to the repository local folder. For example, cd /home/User/ShellRemoteBot
STAGING_DIR=./deb.staging
find "${STAGING_DIR}" -type d -exec chmod 0755 {} \;
find "${STAGING_DIR}" -type f -exec chmod 0644 {} \;
find "${STAGING_DIR}/usr/bin" -type f -exec chmod 0755 {} \;
SIZE_IN_KB="$(du -s ${STAGING_DIR} | awk '{print $1;}')"
echo "Installed-Size: ${SIZE_IN_KB}" >> "${STAGING_DIR}/DEBIAN/control"
gzip -9 -n deb.staging/usr/share/doc/tgshd/changelog.Debian
read -p "Input package filename: " PACKAGE_NAME
dpkg-deb --root-owner-group --build "${STAGING_DIR}" "${PACKAGE_NAME}.deb"