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

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

We keep our protocol, APIs and research spec in separate [protocol][protocol]
repository.

[protocol]: https://github.com/aeternity/protocol

# How to start

We [publish packages][releases] for major platforms on GitHub.
Each release comes with [release notes][release-notes] describing the
changes of the Aeternity node in each particular version.

Please use the [latest published stable release][latest-release] rather than the [`master` branch][master].
The `master` branch tracks the ongoing efforts towards the next stable release to be published though it is not guaranteed to be stable.

[releases]: https://github.com/aeternity/aeternity/releases
[release-notes]: /docs/release-notes
[latest-release]: https://github.com/aeternity/aeternity/releases/latest
[master]: https://github.com/aeternity/aeternity/tree/master

## Quick Install

By using the installer to install the latest stable version:
```bash
bash <(curl -s https://raw.githubusercontent.com/aeternity/installer/v2.0.0/install.sh)
```

Or running a docker container (latest tag):
```bash
docker pull aeternity/aeternity
docker run -p 3013:3013 -p 3015:3015 aeternity/aeternity
```

## Additional resources

* [Threat Model](https://github.com/aeternity/aetmodel/blob/master/ThreatModel.md)


[circleci]: https://circleci.com/gh/aeternity/aeternity
[circleci badge]: https://circleci.com/gh/aeternity/aeternity.svg?style=shield
[appveyor]: https://ci.appveyor.com/project/aeternity/aeternity
[appveyor badge]: https://ci.appveyor.com/api/projects/status/github/aeternity/aeternity?branch=master&svg=true
[license badge]: https://img.shields.io/badge/license-ISC-blue.svg
[license]: https://github.com/aeternity/aeternity/blob/master/LICENSE
[build tool]: https://img.shields.io/badge/build%20tool-rebar3-orange.svg
[rebar3]: https://www.rebar3.org
