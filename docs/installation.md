# Introduction

This document describes how to install an Aeternity node using a release binary either by:

* [Quick install](#quick-install);
* [Install from a package manager](#package-install);
* [Install from a tarball](#tarball-install).

## Quick install

Run below command to install latest version of aeternity node and follow the instructions:

```bash
bash <(curl -s https://install.aeternity.io/install.sh)
```

While this method is the easiest one it is not recomended for production systems.

## Package Install

Installing from a package manager is the recommended way of installing the aeternity node,
as it also automatically installs the additional requirements and makes the updates much more simple.

### Ubuntu

The only supported version so far is 18.04 Bionic (x86-64).

```bash
sudo apt-get install software-properties-common
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D18ABB45C5AF91A0D835367E2106A773A40AFAEB
sudo apt-add-repository 'deb https://apt.aeternity.io bionic main'
sudo apt-get install aeternity-node
sudo service aeternity-node start
```

### macOS

Minimum required version is macOS Mojave 10.14 (x86-64).

```bash
brew tap aeternity/aeternity
brew install aeternity-node
```

## Tarball Install

In order to install an Aeternity node from a tarball, you need to:

* [Retrieve the release tarball](#retrieve-release-tarball) corresponding to your platform;
* [Install the required dependencies](#install-dependencies);
* [Deploy the node](#deploy-node).

### Retrieve release tarball

The release binaries are published on [GitHub][releases] and are tested on the following platforms:

* Ubuntu 18.04 LTS (x86-64);
* macOS Mojave 10.14 (x86-64).

[releases]: https://github.com/aeternity/aeternity/releases

### Install dependencies

Package dependencies are:

* [Libsodium](https://download.libsodium.org/doc/) v1.0.16
* [Openssl](https://www.openssl.org)

#### Ubuntu dependencies

Supported Ubuntu version is 18.04.

The package requires a libsodium v1.0.16 as `libsodium.so.23` shared object/library.
Ubuntu 18.04 ships with libsodium 1.0.16, thus it can be installed with `apt` package manager:

```bash
sudo apt-get install libsodium23
```

The Ubuntu release binaries are built with `libssl1.0.0` (default Ubuntu 18.04 version is 1.1) requirement that can be installed with:

```bash
sudo apt-get install libssl1.0.0
```

#### macOS dependencies

Easiest way to install dependencies is using [Homebrew](https://brew.sh/):

```bash
brew update
brew install openssl libsodium
```

The macOS package has:

* A hard dependency on OpenSSL v1.1 installed with [Homebrew](https://brew.sh/) in its default path `/usr/local/opt/openssl/lib/libcrypto.1.1.dylib`;
* A hard dependency on libsodium v1.0.16 installed with [Homebrew](https://brew.sh/) in its default path `/usr/local/opt/libsodium/lib/libsodium.23.dylib`.

In case you have installed either of them in a non-default path, you could use symlink(s) to work around the issue.
You can create those symlinks by running the following commands:
```bash
ln -s "$(brew --prefix openssl)"/lib/libcrypto.1.1.dylib /usr/local/opt/openssl/lib/libcrypto.1.1.dylib
ln -s "$(brew --prefix libsodium)"/lib/libsodium.23.dylib /usr/local/opt/libsodium/lib/libsodium.23.dylib
```

### Deploy node

In the instructions below, the node is deployed in directory `~/aeternity/node`: you may prefer to deploy the node in an alternative location by amending the instructions accordingly.

It is recommended that the partition where the node directory is has at least 100 GB free: this is needed for the chain and the log files.

Open a Terminal window or get to the command line.
Create a directory and unpack the downloaded package (you need to amend the directory and/or file name of the package):
```bash
mkdir -p ~/aeternity/node
cd ~/aeternity/node
tar xf ~/Downloads/aeternity-<package_version>-macos-x86_64.tar.gz
```
