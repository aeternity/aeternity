#!/bin/bash

set -e

if [ "$(uname -s)" == "Darwin" ]; then
    PKG_SUFFIX="osx-$(sw_vers -productVersion)"
elif [ "$(uname -s)" == "Linux" ]; then
    PKG_SUFFIX="ubuntu-"`uname -m`
fi

VERSION=${CIRCLE_SHA1:-unknown}
if [[ -n $CIRCLE_TAG && $CIRCLE_TAG =~ ^v([0-9]+\.[0-9]+\.[0-9]+)$ ]]; then
    VERSION=${BASH_REMATCH[1]}
fi

export PACKAGE_TARBALL=${PACKAGES_DIR:?}/epoch-${VERSION}-${PKG_SUFFIX}.tar.gz
echo "export PACKAGE_TARBALL=${PACKAGE_TARBALL}" >> $BASH_ENV
