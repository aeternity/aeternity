# Aeternity node

[![CircleCI][circleci badge]][circleci]
[![License][license badge]][license]
[![Build Tool][build tool]][rebar3]

A new blockchain for æpps.

Optimized for scalability via smart contracts inside state-channels.

Has a built-in oracle for integration with real-world data.

Comes with a naming system, for developerability.

Written in Erlang.

To install and run the Aeternity node, see the instructions [below](#how-to-start) or just follow the progress of the project via GitHub Issues.

If you have discovered a bug or security vulnerability please get in touch. The aeternity Crypto Foundation pays bug-bounties up to 100.000 AE Tokens for critical vulnerabilities. Please get in touch via [security@aeternity-foundation.org](mailto:security@aeternity-foundation.org).

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
bash <(curl -s https://install.aeternity.io/install.sh)
```

Or running a docker container (latest tag):
```bash
mkdir -p ~/.aeternity/maindb
docker pull aeternity/aeternity
docker run -p 3013:3013 -p 3015:3015 \
    -v ~/.aeternity/maindb:/home/aeternity/node/data/mnesia \
    aeternity/aeternity
```

#### Restore from snapshot

To speedup the initial blockchain synchronization the node database can be restored from a snapshot following the below steps:

* delete the contents of the database if the node has been started already
* download the database snapshot
* verify if the snapshot checksum match the downloaded file
* unarchive the database snapshot

**Note that the docker container must be stopped before replacing the database**

The following snippet can be used to replace the current database with the latest mainnet snapshot assuming the database path is ` ~/.aeternity/maindb`:

```bash
rm -rf ~/.aeternity/maindb/
curl -o ~/.aeternity/mnesia_main_v-1_latest.tgz https://aeternity-database-backups.s3.eu-central-1.amazonaws.com/mnesia_main_v-1_latest.tgz
CHECKSUM=$(curl https://aeternity-database-backups.s3.eu-central-1.amazonaws.com/mnesia_main_v-1_latest.tgz.md5)
diff -qs <(echo $CHECKSUM) <(openssl md5 -r ~/.aeternity/mnesia_main_v-1_latest.tgz | awk '{ print $1; }')
test $? -eq 0 && tar -xzf ~/.aeternity/mnesia_main_v-1_latest.tgz -C ~/.aeternity/maindb/
```


## Additional resources

* [Threat Model](https://github.com/aeternity/aetmodel/blob/master/ThreatModel.md)


[circleci]: https://circleci.com/gh/aeternity/aeternity
[circleci badge]: https://circleci.com/gh/aeternity/aeternity.svg?style=shield
[license badge]: https://img.shields.io/badge/license-ISC-blue.svg
[license]: https://github.com/aeternity/aeternity/blob/master/LICENSE
[build tool]: https://img.shields.io/badge/build%20tool-rebar3-orange.svg
[rebar3]: https://www.rebar3.org
