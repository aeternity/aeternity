#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

RELEASE_VERSION=${1-}

if [[ -z "${RELEASE_VERSION}" ]]; then
    echo -e "ERROR: No release version given\n"
    echo -e "Usage:\n"
    echo -e "  $0 release_version\n"
    echo "Release version format: x.xx.x"
    echo "You can find a list of epoch releases at https://github.com/aeternity/epoch/releases"
    exit 1
fi

RELEASE_FILE="https://github.com/aeternity/epoch/releases/download/v${RELEASE_VERSION}/epoch-${RELEASE_VERSION}-ubuntu-x86_64.tar.gz"
TEMP_RELEASE_FILE=${TEMP_RELEASE_FILE:=/tmp/epoch.tgz}
TARGET_DIR=${TARGET_DIR:=$HOME/epoch}
EPOCH_CONFIG=${EPOCH_CONFIG:=$TARGET_DIR/epoch.yaml}
NPROC=$(nproc)

echo -e "\nATTENTION: This script will delete the directory ${TARGET_DIR} if it exists. You should back up any contents before continuing.\n"

prerun_choice () {
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
            prerun_choice
            ;;
    esac
}

prerun_choice

echo ""
read -sp "Please provide a secret key passphrase to be used for your keys: " keypassphrase
echo ""

echo -e "\nPrepare host system and install dependencies...\n"
sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get install -y curl libsodium23 libssl1.0.0

echo -e "\nInstall release ${RELEASE_VERSION}...\n"
curl -L "${RELEASE_FILE}" > "${TEMP_RELEASE_FILE}"
rm -rf "${TARGET_DIR}"
mkdir -p "${TARGET_DIR}"
tar -C "${TARGET_DIR}" -xzf "${TEMP_RELEASE_FILE}"

echo -e "\nConfigure release."
echo "NOTE: This might fail if the release version is not compatible with the default configuration. In such a case please refer to the documentation and adapt the configuration accordingly.\n"
cd "${TARGET_DIR}"
BENEFICIARY=$(./bin/epoch keys_gen ${keypassphrase} | awk -F ': ' '{ print $2; }')

cat <<EOF > "${EPOCH_CONFIG}"
sync:
    port: 3115
    external_port: 3015
keys:
    dir: keys
    peer_password: "${keypassphrase}"
http:
    external:
        port: 3013
    internal:
        port: 3113
websocket:
    channel:
        port: 3014
mining:
    beneficiary: "${BENEFICIARY}"
    autostart: true
    cuckoo:
        miner:
            executable: mean30s-generic
            extra_args: "-t ${NPROC}"
            node_bits: 30
chain:
    persist: true
    db_path: ./my_db
EOF

./bin/epoch check_config ${EPOCH_CONFIG}

echo -e "\nClean up...\n"
rm "${TEMP_RELEASE_FILE}"
