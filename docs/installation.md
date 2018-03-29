# Install an epoch node using a release binary

This document describes how to install an epoch node using a release binary.

In order to install an epoch node using a release binary, you need to:
* Retrieve the release binary corresponding to your platform;
* Install the required dependencies;
* Deploy the node.

## Retrieve release binary

The release binaries are published on [GitHub](https://github.com/aeternity/epoch/releases) and are tested on the following platforms:
* Ubuntu 16.04.3 LTS (x86-64);
* macOS Sierra (x86-64);
* macOS High Sierra (x86-64).

## Install dependencies

### Ubuntu 16.04 package

The Ubuntu 16.04 package requires a libsodium shared library (v1.0.16) in `/usr/local/lib/libsodium.so.23` (`wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.16.tar.gz`, unpack, then do `./configure && make && sudo make install && sudo ldconfig`).

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
tar xf ~/Downloads/epoch-0.10.0-osx-10.12.6.tar.gz
```
