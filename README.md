# Aeternity node

[![CircleCI][circleci badge]][circleci]
[![Appveyor][appveyor badge]][appveyor]
[![License][license badge]][license]
[![Build Tool][build tool]][rebar3]

A new blockchain for æpps.

Optimized for scalability via smart contracts inside state-channels.

Has a built-in oracle for integration with real-world data.

Comes with a naming system, for developerability.

Written in Erlang.

To install and run the Aeternity node, see the instructions [below](#how-to-start)
or just [follow the progress of the project in Pivotal Tracker][pivotal].

If you are interested in participating in a security bounty, check our
[HackerOne Aeternity Bounty Program][hackerone].

[pivotal]: https://www.pivotaltracker.com/n/projects/2124891
[hackerone]: https://hackerone.com/aeternity

## What's on board

We keep our protocol, APIs and research spec in separate [protocol][protocol]
repository.

The description of API:
* [Overview of the APIs][api-overview]
* [Intended usage of the user API][api-usage]

[protocol]: https://github.com/aeternity/protocol
[api-overview]: https://github.com/aeternity/protocol/blob/master/node/api/README.md#overview
[api-usage]: https://github.com/aeternity/protocol/blob/master/node/api/README.md#user-api---intended-usage

# How to start

We [publish packages][releases] for major platforms on GitHub.
Each release comes with [release notes][release-notes] describing the
installation and configuration instructions of the Aeternity node.

Below is an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node.

[releases]: https://github.com/aeternity/aeternity/releases
[release-notes]: /docs/release-notes

Please use the [latest published stable release][latest-release] rather than the [`master` branch][master].
The `master` branch tracks the ongoing efforts towards the next stable release to be published though it is not guaranteed to be stable.

[latest-release]: https://github.com/aeternity/aeternity/releases/latest
[master]: https://github.com/aeternity/aeternity/tree/master

## Quick Start

By using the installer to install the latest stable version:
```bash
bash <(curl -s https://raw.githubusercontent.com/aeternity/installer/v2.0.0/install.sh)
```

Or running a docker container (latest tag):
```bash
docker pull aeternity/aeternity
docker run -p 3013:3013 -p 3015:3015 aeternity/aeternity
```

## Installation

See the documenation on how to:
* [install the Aeternity node from the release binary][installation-release-binary]
* [install the Aeternity node on Windows][installation-windows]
* [install CUDA miner on Ubuntu][installation-cuda-miner]

[installation-release-binary]: /docs/installation.md
[installation-windows]: /docs/installation-windows.md
[installation-cuda-miner]: /docs/cuda-miner.md

## Building from source

See [the documentation on how to build the Aeternity node from source][building-from-source].

[building-from-source]: /docs/build.md

## Configuration

See [the documentation on how to configure the Aeternity node][configuration].

[configuration]: /docs/configuration.md

## Running the node

See [the documentation on how to operate the Aeternity node][operation].

[operation]: /docs/operation.md

## Docker

See [the documentation on how to run the Aeternity node if you prefer Docker][docker].

[docker]: /docs/docker.md

# Additional resources

* [Threat Model](https://github.com/aeternity/aetmodel/blob/master/ThreatModel.md)


[circleci]: https://circleci.com/gh/aeternity/aeternity
[circleci badge]: https://circleci.com/gh/aeternity/aeternity.svg?style=shield
[appveyor]: https://ci.appveyor.com/project/aeternity/aeternity
[appveyor badge]: https://ci.appveyor.com/api/projects/status/github/aeternity/aeternity?branch=master&svg=true
[license badge]: https://img.shields.io/badge/license-ISC-blue.svg
[license]: https://github.com/aeternity/aeternity/blob/master/LICENSE
[build tool]: https://img.shields.io/badge/build%20tool-rebar3-orange.svg
[rebar3]: https://www.rebar3.org
