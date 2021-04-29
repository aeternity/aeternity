# Introduction

This document describes how to build an Aeternity node from source on:

- Ubuntu 18.04 LTS
- Ubuntu 20.04 LTS
- MacOS (latest)
- Archlinux 20210404
- openSUSE Leap 15.2

While the package should build on most linux distributions that's not verified (in CI) for each release on other than the below platforms:

- Ubuntu 18.04 LTS
- MacOS (latest)

The commands below assume you are logged in with `sudo` user.

The node have couple of main dependencies that have to be installed to build it from source:

- [Erlang/OTP](http://erlang.org/doc/installation_guide/INSTALL.html)
- [Libsodium](https://download.libsodium.org/doc/installation/)
- [Libgmp](https://gmplib.org)

## Dependencies

### Ubuntu 18.04/20.04

Update package database, packages and install the common tools and libraries:

```bash
sudo apt-get -qq update \
&& sudo apt-get -y upgrade \
&& sudo apt-get -qq -y install git autoconf build-essential cmake erlang libsodium-dev libgmp-dev
```

### MacOS

The easiest way to install package on MacOS is Homebrew, it can be installed by running:

```bash
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Then install the build dependencies using the `brew` command:
```
brew update
brew install erlang@21 openssl libsodium autoconf gmp
```

### Archlinux

Update package database, packages and install the common tools and libraries:

```bash
sudo pacman -Sy
sudo pacman -S git base-devel cmake ncurses erlang22-nox libsodium gmp
```

### openSUSE Leap 15.2

```bash
sudo zypper install -t pattern devel_basis
sudo zypper install cmake gcc-c++ git erlang libsodium-devel gmp-devel
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

If `prod-build` went fine, configuration is in place, one should be able to navigate to the build artifacts directory and start the Aeternity node:

```bash
cd _build/prod/rel/aeternity/
bin/aeternity daemon
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

Then start the node in background mode:

```bash
~/aeternity/node/bin/aeternity daemon
```

See [operation documentation](operation.md) for more details.
