# Install an epoch node using a release binary

This document describes how to install an epoch node using a release binary.

In order to install an epoch node using a release binary, you need to:
* Retrieve the release binary corresponding to your platform;
* Install the required dependencies;
* Deploy the node.

## Retrieve release binary

The release binaries are published on [GitHub](https://github.com/aeternity/epoch/releases) and are tested on the following platforms:
* Ubuntu 16.04.3 LTS (x86-64);
* Ubuntu 18.04 LTS (x86-64);
* macOS Sierra (x86-64);
* macOS High Sierra (x86-64).

## Install dependencies

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
wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.16.tar.gz
tar -xf libsodium-1.0.16.tar.gz && cd libsodium-1.0.16
./configure && make && sudo make install && sudo ldconfig
```

### macOS package

The macOS package has:
* A hard dependency on OpenSSL v1.0.0 installed with [Homebrew](https://brew.sh/) in its default path `/usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib`;
* A hard dependency on libsodium v1.0.16 installed with [Homebrew](https://brew.sh/) in its default path `/usr/local/opt/libsodium/lib/libsodium.23.dylib`.

In case you have installed either of them in a non-default path, you could use symlink(s) to work around the issue.

## Deploy node

In the instructions below, the node is deployed in directory `/tmp/node`: you may prefer to deploy the node in an alternative (and less ephemeral) location - e.g. a `node` directory inside your home directory - by amending the instructions accordingly.
It is recommended that the partition where the node directory is has at least 10 GB free: this is needed for the chain and the log files.

Open a Terminal window or get to the command line.

Create a directory and unpack the downloaded package (you may need to amend the directory and/or file name of the package):
```bash
mkdir /tmp/node
cd /tmp/node
tar xf ~/Downloads/epoch-0.19.0-osx-10.12.6.tar.gz
```
