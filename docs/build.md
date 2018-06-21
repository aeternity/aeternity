# Build from source

This document describes how to build an epoch node from source on current Ubuntu 16.04.4 LTS or Ubuntu 18.04 LTS.
The commands below assume you are logged in with `sudo` user.

## Dependencies installation

### Common tools and libraries

Make sure your Ubuntu version and it's packages are up to date, then install required tools and libraries:
```bash
sudo apt-get -qq update \
&& sudo apt-get -y upgrade \
&& sudo apt-get -qq -y install git curl autoconf build-essential ncurses-dev libssl-dev
```

### OTP install

Required Erlang OTP version is `20.1`.

A compatible OTP version can be installed from a package both on Ubuntu 16.04 and Ubuntu 18.04:
```bash
sudo apt-get install erlang
```

### Libsodium install

Required Libsodium version is `1.0.16`.

#### Ubuntu 18.04

Since Ubuntu 18.04 ships with libsodium version 1.0.16 it can be installed from apt package:

```bash
sudo apt-get install libsodium-dev
```

#### Ubuntu 16.04

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
&& sudo ldconfig
&& cd ..
```

For more details read the [dedicated Libsodium documentation](https://download.libsodium.org/doc/installation/).

## Builds

Epoch source code can be obtained by cloning the public [GitHub repository](https://github.com/aeternity/epoch):

```bash
git clone https://github.com/aeternity/epoch.git epoch && cd epoch
```

Identify the version to be built:
```
VERSION=0.17.0
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

If everything went fine, one should be able to navigate to the build artifacts directory and start the epoch node:
```bash
cd _build/prod/rel/epoch/
bin/epoch start
```

See [operation documentation](operation.md) for more details.

### Production package

Alternatively a production package similar to what is distributed via [GitHub releases](https://github.com/aeternity/epoch/releases) can be created by running:

```bash
make prod-package
```

Once the packaging is done, the package is created in the `_build/prod/rel/epoch/` directory, e.g. `_build/prod/rel/epoch/epoch-${VERSION:?}.tar.gz`.

To deploy the package for example in `/tmp/node` one should just unarchive it to that directory:

```bash
mkdir /tmp/node
tar xf _build/prod/rel/epoch/epoch-${VERSION:?}.tar.gz -C /tmp/node
```

Then start the node:
```bash
/tmp/node/bin/epoch start
```

See [operation documentation](operation.md) for more details.
