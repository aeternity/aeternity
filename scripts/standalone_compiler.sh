#!/usr/bin/env bash

MD5="d40bf7e90120eedd6ea04aa897c34a1c"
DST="_build/test/lib/aecontract/test/bin"
SRC="https://github.com/aeternity/aesophia/releases/download/roma-v1/aesophia"

function check() {
    if [ -e ${DST}/aesophia ]; then
        if [ ! -x ${DST}/aesophia ]; then
            chmod +x ${DST}/aesophia
        fi
        MD5X=`openssl md5 ${DST}/aesophia | cut -d' ' -f2`
        if [ ${MD5} = ${MD5X} ]; then
            echo "All is good, standalone compiler in place"
            exit 0
        fi
    fi
}

check

echo "Installing standalone ROMA compiler"

if [ ! -e ${DST} ]; then
    mkdir -p ${DST}
fi

curl -s -L ${SRC} -o ${DST}/aesophia

if [ $? -gt 0 ]; then
    echo "ERROR: failed to fetch ${SRC}"
    exit 1
fi

check

echo "ERROR: Wrong MD5 for ${DST}"
exit 1
