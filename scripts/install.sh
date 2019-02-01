#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

RELEASE_VERSION=${1-}

if [[ -z "${RELEASE_VERSION}" ]]; then
    echo -e "ERROR: No release version given\n"
    echo -e "Usage:\n"
    echo -e "  $0 release_version\n"
    echo "Release version format is X.Y.Z where X, Y, and Z are non-negative integers"
    echo "You can find a list of aeternity releases at https://github.com/aeternity/aeternity/releases"
    exit 1
fi

TEMP_RELEASE_FILE=${TEMP_RELEASE_FILE:=/tmp/aeternity.tgz}
TARGET_DIR=${TARGET_DIR:=$HOME/aeternity/node}
PACKAGE_PREFIX=aeternity
MACOS_PACKAGE_SUFFIX=macos-x86_64
declare -a OLD_RELEASE_VERSIONS=("1.0.0" "1.0.1" "1.1.0" "1.2.0");

in_array() {
    local haystack=${1}[@]
    local needle=${2}
    for i in ${!haystack}; do
        if [[ ${i} == ${needle} ]]; then
            return 0
        fi
    done
    return 1
}

install_prompt () {
    echo -e "\nATTENTION: This script will delete the directory ${TARGET_DIR} if it exists. You should back up any contents before continuing.\n"
    read -p "Continue (y/n)?" inputprerunchoice
    case "$inputprerunchoice" in
        y|Y )
            echo "Continuing..."
            ;;
        n|N )
            echo "Exiting..."
            exit 0
            ;;
        * )
            echo "Invalid input..."
            install_prompt
            ;;
    esac
}

install_deps_ubuntu() {
    OS_RELEASE=$(lsb_release -r -s)
    echo -e "\nPrepare host system and install dependencies ...\n"
    sudo apt-get update
    sudo apt-get upgrade -y
    sudo apt-get install -y curl libssl1.0.0

    if [[ "$OS_RELEASE" = "16.04" ]]; then
        sudo apt-get install -y build-essential
        LIB_VERSION=1.0.16
        wget https://download.libsodium.org/libsodium/releases/libsodium-${LIB_VERSION}.tar.gz
        tar -xf libsodium-${LIB_VERSION}.tar.gz && cd libsodium-${LIB_VERSION} &&
        ./configure && make && sudo make install && sudo ldconfig
        cd .. && rm -rf libsodium-${LIB_VERSION} && rm libsodium-${LIB_VERSION}.tar.gz
    elif [[ "$OS_RELEASE" = "18.04" ]]; then
        sudo apt-get install -y curl libsodium23
    else
        echo -e "Unsupported Ubuntu version! Please refer to the documentation for supported versions."
        exit 1
    fi
}

install_deps_osx() {
    VER=$(sw_vers -productVersion)

    if ! [[ "$VER" = "10.13"* || $VER = "10.14"* ]]; then
        echo -e "Unsupported OSX version! Please refer to the documentation for supported versions."
        exit 1
    fi

    echo -e "\nInstalling dependencies ...\n"
    brew update
    brew install openssl libsodium
}

install_node() {
    install_prompt
    RELEASE_FILE=$1
    echo -e "\nInstalling release ${RELEASE_VERSION} ...\n"
    curl -L "${RELEASE_FILE}" > "${TEMP_RELEASE_FILE}"
    rm -rf "${TARGET_DIR}"
    mkdir -p "${TARGET_DIR}"
    tar -C "${TARGET_DIR}" -xzf "${TEMP_RELEASE_FILE}"

    echo -e "\nCleanup...\n"
    rm "${TEMP_RELEASE_FILE}"
}

# Backward compatibility package names
# @TODO remove after 2.* release
if in_array OLD_RELEASE_VERSIONS $RELEASE_VERSION; then
    PACKAGE_PREFIX=epoch
    MACOS_PACKAGE_SUFFIX=osx-10.13.6
fi

if [[ "$OSTYPE" = "linux-gnu" && $(lsb_release -i -s) = "Ubuntu" ]]; then
    install_deps_ubuntu
    install_node "https://github.com/aeternity/aeternity/releases/download/v${RELEASE_VERSION}/${PACKAGE_PREFIX}-${RELEASE_VERSION}-ubuntu-x86_64.tar.gz"
elif [[ "$OSTYPE" = "darwin"* ]]; then
    install_deps_osx
    install_node "https://github.com/aeternity/aeternity/releases/download/v${RELEASE_VERSION}/${PACKAGE_PREFIX}-${RELEASE_VERSION}-${MACOS_PACKAGE_SUFFIX}.tar.gz"
else
    echo -e "Unsupported platform (OS)! Please refer to the documentation for supported platforms."
    exit 1
fi

echo -e "Installation completed."
echo -e "Run '${TARGET_DIR}/bin/epoch start' to start the node"
