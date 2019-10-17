# Introduction

This document describes how to install an Aeternity node using a release binary either by:

* [Quick install](#quick-install);
* Manual install.

In order to install an Aeternity node manually, you need to:

* [Retrieve the release binary](#retrieve-release-binary) corresponding to your platform;
* [Install the required dependencies](#install-dependencies);
* [Deploy the node](#deploy-node).

## Quick install

Run below command to install latest version of aeternity node and follow the instructions:

```bash
bash <(curl -s https://install.aeternity.io/install.sh)
```

## Retrieve release binary

The release binaries are published on [GitHub](https://github.com/aeternity/aeternity/releases) and are tested on the following platforms:

* Ubuntu 16.04.3 LTS (x86-64);
* Ubuntu 18.04 LTS (x86-64);
* macOS High Sierra 10.13 (x86-64);
* macOS Mojave 10.14 (x86-64).

## Install dependencies

Package dependencies are:

* [Libsodium](https://download.libsodium.org/doc/) v1.0.16
* [Openssl](https://www.openssl.org) 1.0.0

### Ubuntu package

The package requires a libsodium v1.0.16 as `libsodium.so.23` shared object/library.

#### Ubuntu 18.04

Ubuntu 18.04 ships with libsodium 1.0.16, thus it can be installed with `apt` package manager:

```bash
sudo apt-get install libsodium23
```

The Ubuntu release binaries are built with `libssl1.0.0` (default Ubuntu 18.04 version is 1.1) requirement that can be installed with:

```bash
sudo apt-get install libssl1.0.0
```

#### Ubuntu 16.04

As Ubuntu 16.04 ships with older libsodium version than required, it must be installed from source.
A C compiler and related tools must be installed beforehand by running:

```bash
sudo apt-get install build-essential
```

then the library:

```bash
curl -O https://download.libsodium.org/libsodium/releases/libsodium-1.0.16.tar.gz
tar -xf libsodium-1.0.16.tar.gz && cd libsodium-1.0.16
./configure && make && sudo make install && sudo ldconfig
```

### macOS package

Easiest way to install dependencies is using [Homebrew](https://brew.sh/):
```bash
brew update
brew install openssl libsodium
```

The macOS package has:

* A hard dependency on OpenSSL v1.0.0 installed with [Homebrew](https://brew.sh/) in its default path `/usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib`;
* A hard dependency on libsodium v1.0.16 installed with [Homebrew](https://brew.sh/) in its default path `/usr/local/opt/libsodium/lib/libsodium.23.dylib`.

In case you have installed either of them in a non-default path, you could use symlink(s) to work around the issue.
You can create those symlinks by running the following commands:
```bash
ln -s "$(brew --prefix openssl)"/lib/libcrypto.1.0.0.dylib /usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib
ln -s "$(brew --prefix libsodium)"/lib/libsodium.23.dylib /usr/local/opt/libsodium/lib/libsodium.23.dylib
```

## Deploy node

In the instructions below, the node is deployed in directory `~/aeternity/node`: you may prefer to deploy the node in an alternative location by amending the instructions accordingly.

It is recommended that the partition where the node directory is has at least 40 GB free: this is needed for the chain and the log files.

Open a Terminal window or get to the command line.
Create a directory and unpack the downloaded package (you may need to amend the directory and/or file name of the package):
```bash
mkdir -p ~/aeternity/node
cd ~/aeternity/node
tar xf ~/Downloads/aeternity-5.0.0-rc.5-macos-x86_64.tar.gz
```
