#!/bin/bash

set -e

if [ "$(uname -s)" == "Darwin" ]; then
    PKG_SUFFIX="macos-"`uname -m`
elif [ "$(uname -s)" == "Linux" ]; then
    PKG_SUFFIX="ubuntu-"`uname -m`
fi

VERSION=${CIRCLE_SHA1:-unknown}
if [[ -n $CIRCLE_TAG && $CIRCLE_TAG =~ ^v([0-9]+\.[0-9]+\.[0-9]+(-[a-z0-9]+)*)$ ]]; then
    VERSION=${BASH_REMATCH[1]}
fi

PACKAGE_TARBALL=${PACKAGES_DIR:?}/aeternity-${VERSION}-${PKG_SUFFIX}.tar.gz
echo "export PKG_SUFFIX=${PKG_SUFFIX}" >> $BASH_ENV
echo "export PACKAGE_TARBALL=${PACKAGE_TARBALL}" >> $BASH_ENV
