# Introduction

This document describes how to build an Aeternity node from source on current Ubuntu 16.04.4 LTS, Ubuntu 18.04 LTS and MacOS (latest).
The commands below assume you are logged in with `sudo` user.

The node have couple of main dependencies that have to be install to build it from source:

- [Erlang/OTP](http://erlang.org/doc/installation_guide/INSTALL.html)
- [Libsodium](https://download.libsodium.org/doc/installation/)

## Dependencies

### Ubuntu 18.04

Update package database, packages and install the common tools and libraries:

```bash
sudo apt-get -qq update \
&& sudo apt-get -y upgrade \
&& sudo apt-get -qq -y install git autoconf build-essential erlang libsodium-dev
```

### Ubuntu 16.04

Update package database, packages and install the common tools and libraries:

```bash
sudo apt-get -qq update \
&& sudo apt-get -y upgrade \
&& sudo apt-get -qq -y install git curl autoconf build-essential ncurses-dev libssl-dev
```

As Ubuntu 16.04 ships with outdated erlang and libsodium versions, they have to be installed from source:

```bash
OTP_VERSION="20.2.3"
OTP_DOWNLOAD_URL="https://github.com/erlang/otp/archive/OTP-${OTP_VERSION}.tar.gz"
curl -fsSL -o otp-src.tar.gz "$OTP_DOWNLOAD_URL" \
&& mkdir otp-src \
&& tar -zxf otp-src.tar.gz -C otp-src --strip-components=1 \
&& cd otp-src \
&& export ERL_TOP=`pwd` \
&& ./otp_build autoconf && ./configure && make -j$(nproc) && sudo make install \
&& cd ..
```

```bash
LIBSODIUM_VERSION="1.0.16"
LIBSODIUM_DOWNLOAD_URL="https://github.com/jedisct1/libsodium/releases/download/${LIBSODIUM_VERSION}/libsodium-${LIBSODIUM_VERSION}.tar.gz"
curl -fsSL -o libsodium-src.tar.gz "$LIBSODIUM_DOWNLOAD_URL" \
&& mkdir libsodium-src \
&& tar -zxf libsodium-src.tar.gz -C libsodium-src --strip-components=1 \
&& cd libsodium-src \
&& ./configure && make -j$(nproc) \
&& sudo make install \
&& sudo ldconfig \
&& cd ..
```

### MacOS

The easiest way to install package on MacOS is Homebrew, it can be installed by running:

```bash
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Then install the build dependencies using the `brew` command:
```
brew update
brew install erlang openssl libsodium autoconf
```

## Building

The source code of the Aeternity node can be obtained by cloning the public [GitHub repository](https://github.com/aeternity/aeternity):

```bash
git clone https://github.com/aeternity/aeternity.git aeternity_source && cd aeternity_source
```

**NOTE**: By default git will checkout the `master` (default) branch of the source code.
To build a particular version it should be checkout first:


```bash
VERSION=X.Y.Z # set a particular version
git checkout tags/v${VERSION:?}
```


### Production build

One can create a production build by running:
```bash
make prod-build
```

Make sure beneficiary account is set in configuration, as this is mandatory to successfully start a node.
There is no default beneficiary configured.

See [configuration documentation](configuration.md) for configuration details.

If `prod-build` went fine, configuration is in place, one should be able to navigate to the build artifacts directory and start the Aeternity node:
```bash
cd _build/prod/rel/aeternity/
bin/aeternity start
```

See [operation documentation](operation.md) for more details.

### Production package

Alternatively a production package similar to what is distributed via [GitHub releases](https://github.com/aeternity/aeternity/releases) can be created by running:

```bash
make prod-package
```

Once the packaging is done, the package is created in the `_build/prod/rel/aeternity/` directory, e.g. `_build/prod/rel/aeternity/aeternity-X.Y.Z.tar.gz`.

To deploy the package for example in `~/aeternity/node` one should just unarchive it to that directory:

```bash
mkdir -p ~/aeternity/node
tar xf _build/prod/rel/aeternity/aeternity-*.tar.gz -C ~/aeternity/node
```

Make sure beneficiary account is set in configuration, as this is mandatory to successfully start a node.
There is no default beneficiary configured.

See [configuration documentation](configuration.md) for configuration details.

Then start the node:
```bash
~/aeternity/node/bin/aeternity start
```

See [operation documentation](operation.md) for more details.
