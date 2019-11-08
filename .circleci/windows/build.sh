#!/usr/bin/env bash

# Build release packages into $PACKAGES_PATH
# $PROJECT_ROOT - project dir or run the scripts from project root
# $PACKAGES_PATH - target dir
# $WORK_ROOT
# $RELEASE_PATH

set -e

export MSYS=winsymlinks:nativestrict

PROJECT_ROOT=${PROJECT_ROOT:-`pwd`}
PACKAGES_PATH=${PACKAGES_PATH:-$1}
PACKAGES_PATH=${PACKAGES_PATH:-${PROJECT_ROOT}/packages}
WORK_ROOT=${WORK_ROOT:-/tmp/win_package_build}
WORK_PATH=${WORK_ROOT}/aeternity-windows-w64
RELEASE_PATH=${RELEASE_PATH:-${PROJECT_ROOT}/_build/prod/rel/aeternity}

bold="\\e[1;33m"
reset="\\e[0m"

echo -e "\\e[1;34mTarget path ${PACKAGES_PATH}${reset}"

echo -e "${bold}Remove any previous package environment${reset}"
rm -rf ${WORK_ROOT}
mkdir -p ${WORK_ROOT}

echo -e "${bold}Start build${reset}"
cd ${PROJECT_ROOT}
time make prod-build

echo -e "${bold}Create package environment${reset}"
NODE_VERSION=`cat ${PROJECT_ROOT}/VERSION`
sed "s/VERSION_PLACEHOLDER/${NODE_VERSION}/g" .circleci/windows/package.cfg > ${WORK_ROOT}/package.cfg

time styrene -o ${WORK_ROOT} ${WORK_ROOT}/package.cfg

echo -e "${bold}Copy release into package environment${reset}"

mkdir -p ${WORK_PATH}/usr/lib
time cp -R ${RELEASE_PATH} ${WORK_PATH}/usr/lib/
mv -f ${WORK_PATH}/usr/lib/aeternity/REVISION ${WORK_PATH}/
mv -f ${WORK_PATH}/usr/lib/aeternity/VERSION ${WORK_PATH}/

# Remove erl.ini files from release
find ${WORK_PATH}/usr/lib -name erl.ini -type f -delete

# Copy genesis and hard-fork account migrations into top-level data folder
mkdir -p ${WORK_PATH}/data
cp -R ${RELEASE_PATH}/data/aecore ${WORK_PATH}/data/

echo -e "${bold}Build packages${reset}"

time styrene -o ${WORK_ROOT} ${WORK_ROOT}/package.cfg

echo -e "${bold}Copy artifacts to $PACKAGES_PATH${reset}"

VERSION=${CIRCLE_SHA1:-unknown}
if [[ -n $CIRCLE_TAG && $CIRCLE_TAG =~ ^v([0-9]+\.[0-9]+\.[0-9]+(-[a-z0-9\.\+]+)*)$ ]]; then
    VERSION=${BASH_REMATCH[1]}
fi

mkdir -p ${PACKAGES_PATH}
cp ${WORK_ROOT}/aeternity-*.zip ${PACKAGES_PATH}/aeternity-${VERSION}-windows-x86_64-experimental.zip
cp ${WORK_ROOT}/aeternity-*.exe ${PACKAGES_PATH}/aeternity-${VERSION}-windows-x86_64-experimental.exe
echo -e "${bold}Done.${reset}"
