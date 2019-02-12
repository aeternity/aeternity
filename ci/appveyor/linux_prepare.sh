#!/usr/bin/env bash

sudo apt update
# sudo apt-get upgrade -y
sudo apt install -y git curl libssl1.0.0 build-essential libsodium23 libsodium-dev autoconf ncurses-dev libssl-dev

PACKAGE_NAME=esl-erlang_${OTP_VERSION}-1~ubuntu~bionic_amd64.deb
OTP_DOWNLOAD_URL=https://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/${PACKAGE_NAME}
curl -fsSL -o /tmp/${PACKAGE_NAME} "$OTP_DOWNLOAD_URL"
sudo apt remove -y erlang-base
sudo apt autoremove -y
sudo apt install -y libwxbase3.0-0v5 libwxgtk3.0-0v5 libsctp1
sudo dpkg -i /tmp/${PACKAGE_NAME} || true
sudo apt --fix-broken install -y || true
sudo dpkg -i /tmp/${PACKAGE_NAME}
