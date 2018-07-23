#!/bin/sh

if [ "$(uname -s)" == "Darwin" ]; then
    PKG_SUFFIX="osx-$(sw_vers -productVersion)"
elif [ "$(uname -s)" == "Linux" ]; then
    PKG_SUFFIX="ubuntu-"`uname -m`
fi

VERSION=${CIRCLE_SHA1:-unknown}
if test -n $CIRCLE_TAG; then
    MATCH=$(echo $CIRCLE_TAG | grep -E "^v([0-9]+\.[0-9]+\.[0-9]+)$")
    if test $? -eq 0; then
        VERSION=$MATCH
    fi
fi

PACKAGE_TARBALL=${PACKAGES_DIR:?}/epoch-${VERSION}-${PKG_SUFFIX}.tar.gz
echo "export PACKAGE_TARBALL=${PACKAGE_TARBALL}" >> $BASH_ENV
