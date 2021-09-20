#!/bin/bash

set -e

if [ -z "${PKG_TARGET_OS}" ]; then
    if [ "$(uname -s)" == "Darwin" ]; then
        PKG_TARGET_OS="macos"
    elif [ "$(uname -s)" == "Linux" ]; then
        PKG_TARGET_OS="ubuntu"
    fi
fi

PKG_SUFFIX="${PKG_TARGET_OS}-"`uname -m`

if [ -n "${PKG_KIND}" ]; then
    PKG_SUFFIX="${PKG_SUFFIX}-${PKG_KIND}"
fi

VERSION=$(git rev-parse HEAD)
if [[ -n $CIRCLE_TAG && $CIRCLE_TAG =~ ^v([0-9]+\.[0-9]+\.[0-9]+(-[a-z0-9\.\+]+)*)$ ]]; then
    VERSION=${BASH_REMATCH[1]}
fi

PACKAGE_TARBALL=${PACKAGES_DIR:?}/aeternity-${VERSION}-${PKG_SUFFIX}${PKG_EXT:-".tar.gz"}
echo "export PKG_SUFFIX=${PKG_SUFFIX}" >> $BASH_ENV
echo "export PACKAGE_TARBALL=${PACKAGE_TARBALL}" >> $BASH_ENV
