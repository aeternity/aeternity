# Build from source

This document describes how to build an Aeternity node from source on current Ubuntu 16.04.4 LTS, Ubuntu 18.04 LTS and MacOS (latest).
The commands below assume you are logged in with `sudo` user.

## Dependencies installation

### Ubuntu
#### Common tools and libraries

Make sure your Ubuntu version and it's packages are up to date, then install required tools and libraries:
```bash
sudo apt-get -qq update \
&& sudo apt-get -y upgrade \
&& sudo apt-get -qq -y install git curl autoconf build-essential ncurses-dev libssl-dev
```

#### OTP install

Required Erlang OTP version is `20.1`.

##### Ubuntu 18.04

```bash
sudo apt-get install erlang
```

##### Ubuntu 16.04

Ubuntu 16.04 ships with outdated erlang version. Version 20 can be installed from source:

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

#### Libsodium install

Required Libsodium version is `1.0.16`.

##### Ubuntu 18.04

Since Ubuntu 18.04 ships with libsodium version 1.0.16 it can be installed from apt package:

```bash
sudo apt-get install libsodium-dev
```

##### Ubuntu 16.04

Ubuntu 16.04 ships with older than required version of libsodium thus it must be installed from source running below commands:

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

Install `brew` if not done yet:
```
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```
Also, install dependencies:
```
brew update
brew install erlang
brew install openssl libsodium
brew install autoconf
```

For more details read the [dedicated Libsodium documentation](https://download.libsodium.org/doc/installation/).

## Builds

The source code of the Aeternity node can be obtained by cloning the public [GitHub repository](https://github.com/aeternity/aeternity):

```bash
git clone https://github.com/aeternity/aeternity.git aeternity_source && cd aeternity_source
```

Identify the version to be built:
```
VERSION=2.4.0
```

Checkout the version to be built:

```bash
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

Once the packaging is done, the package is created in the `_build/prod/rel/aeternity/` directory, e.g. `_build/prod/rel/aeternity/aeternity-${VERSION:?}.tar.gz`.

To deploy the package for example in `~/aeternity/node` one should just unarchive it to that directory:

```bash
mkdir -p ~/aeternity/node
tar xf _build/prod/rel/aeternity/aeternity-${VERSION:?}.tar.gz -C ~/aeternity/node
```

Make sure beneficiary account is set in configuration, as this is mandatory to successfully start a node.
There is no default beneficiary configured.

See [configuration documentation](configuration.md) for configuration details.

Then start the node:
```bash
~/aeternity/node/bin/aeternity start
```

See [operation documentation](operation.md) for more details.
