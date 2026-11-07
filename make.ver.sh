#!/usr/bin/env bash

# Single source of version: derives the project version from the latest
# numeric git tag and the commit distance to it (git describe --tags --long),
# then writes it into deb.staging/DEBIAN/control (Version) and src/tgshd.lpi
# (VersionInfo: Major/Minor/Revision/BuildNr), so the two cannot drift apart.
# Prints the resulting Debian version (e.g. 1.2.5.6-1) on stdout.
#
# Tag forms: X, X.Y, X.Y.Z, X.Y.Z.B (optionally with a 'v' prefix). The 4th
# component (BuildNr) is the commit distance to the tag; for 4-component
# tags it is the tag's own build number plus the distance.
set -eu

declare DESC TAG='' DIST=0
if DESC=$(git describe --tags --long --match '[0-9]*' --match 'v[0-9]*' 2>/dev/null); then
    if ! [[ "${DESC}" =~ ^v?(.*)-([0-9]+)-g[0-9a-fA-F]+$ ]]; then
        echo "Cannot parse git describe output: ${DESC}" >&2
        exit 1
    fi
    TAG=${BASH_REMATCH[1]}
    DIST=${BASH_REMATCH[2]}
else
    echo "Warning: git describe failed, falling back to version 0.0.0.0" >&2
fi

declare -a PARTS=()
IFS='.' read -ra PARTS <<< "${TAG}"
declare MAJOR=${PARTS[0]:-0} MINOR=${PARTS[1]:-0} REVISION=${PARTS[2]:-0}
if ((${#PARTS[@]} > 3)); then
    declare BUILD=$((PARTS[3] + DIST))
else
    declare BUILD=${DIST}
fi
declare VERSION="${MAJOR}.${MINOR}.${REVISION}.${BUILD}"

# Debian control: Version line (Debian revision stays -1)
grep -q '^Version: ' deb.staging/DEBIAN/control
sed --in-place "s/^Version: .*/Version: ${VERSION}-1/" deb.staging/DEBIAN/control

# Lazarus project: rewrite the VersionInfo block (keeping the .lpi line
# endings); AutoIncrementBuild is disabled so the version stays
# deterministic between builds
grep -q '<VersionInfo>' src/tgshd.lpi
declare CR=''
if grep -q $'\r' src/tgshd.lpi; then
    CR=$'\r'
fi
awk -v "CR=${CR}" -v "MAJOR=${MAJOR}" -v "MINOR=${MINOR}" \
    -v "REVISION=${REVISION}" -v "BUILD=${BUILD}" '
    /<VersionInfo>/ {
        print "    <VersionInfo>" CR;
        print "      <UseVersionInfo Value=\"True\"/>" CR;
        print "      <AutoIncrementBuild Value=\"False\"/>" CR;
        print "      <MajorVersionNr Value=\"" MAJOR "\"/>" CR;
        print "      <MinorVersionNr Value=\"" MINOR "\"/>" CR;
        print "      <RevisionNr Value=\"" REVISION "\"/>" CR;
        print "      <BuildNr Value=\"" BUILD "\"/>" CR;
        skip = 1;
        next
    }
    /<\/VersionInfo>/ {
        skip = 0
    }
    skip == 0 {
        print
    }
' src/tgshd.lpi > src/tgshd.lpi.tmp
mv --force src/tgshd.lpi.tmp src/tgshd.lpi

echo "${VERSION}-1"
