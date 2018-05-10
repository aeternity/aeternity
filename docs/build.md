# Build from source

This document describes how to build an epoch node from source on current Ubuntu 16.04.4 LTS.
The commands below assume you are logged in with `sudo` user.

## Dependencies installation

### Tools and libraries

Make sure your Ubuntu version and it's packages are up to date, then install required tools and libs:
```bash
sudo apt-get -qq update \
&& sudo apt-get -y upgrade \
&& sudo apt-get -qq -y install \
git curl autoconf build-essential ncurses-dev libssl-dev
```

### OTP install

Required Erlang OTP version is `20.2.3` it could be installed from source running below commands:

```bash
OTP_VERSION="20.2.3"
OTP_DOWNLOAD_URL="https://github.com/erlang/otp/archive/OTP-${OTP_VERSION}.tar.gz"
curl -fsSL -o otp-src.tar.gz "$OTP_DOWNLOAD_URL" \
&& mkdir otp-src \
&& tar -zxf otp-src.tar.gz -C otp-src --strip-components=1 \
&& cd otp-src \
&& export ERL_TOP=`pwd` \
&& ./otp_build autoconf && ./configure && make -j$(nproc) \
&& sudo make install
&& cd ..
```

For more details read the [dedicated OTP documentation](http://erlang.org/doc/installation_guide/INSTALL.html).

### Libsodium install

Required Libsodium version is `1.0.16` it could be installed from source running below commands:

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

Then checkout the version to be build, e.g. `v0.13.0`:

```bash
git checkout tags/v0.13.0
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

Once the packaging is done, assuming the built version is `v0.13.0`, the package is created as `_build/prod/rel/epoch/epoch-0.13.0.tar.gz`.

To deploy the package for example in `/tmp/node` one should just unarchive it to that directory:

```bash
mkdir /tmp/node
tar xf _build/prod/rel/epoch/epoch-0.13.0.tar.gz -C /tmp/node
```

Then start the node:
```bash
/tmp/node/bin/epoch start
```

See [operation documentation](operation.md) for more details.
