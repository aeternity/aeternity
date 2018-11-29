# Aeternity Epoch

A new blockchain for æpps.

Optimized for scalability via smart contracts inside state-channels.

Has a built-in oracle for integration with real-world data.

Comes with a naming system, for developerability.

Written in Erlang.

To install and run the epoch node, see the [release notes][release-notes]
or just [follow the progress of the project in Pivotal Tracker][pivotal].

If you are interested in participating in a security bounty, check our
[HackerOne Aeternity Bounty Program][hackerone].

[release-notes]: /docs/release-notes
[pivotal]: https://www.pivotaltracker.com/n/projects/2124891
[hackerone]: https://hackerone.com/aeternity

## What's on board

We keep our protocol, APIs and research spec in separate [protocol][protocol]
repository.

The description of API:
* [Overview of the APIs][api-overview]
* [Intended usage of the user API][api-usage]

[protocol]: https://github.com/aeternity/protocol
[api-overview]: https://github.com/aeternity/protocol/blob/master/epoch/api/README.md#overview
[api-usage]: https://github.com/aeternity/protocol/blob/master/epoch/api/README.md#user-api---intended-usage

# How to start

We [publish packages][packages] for major platforms on GitHub.

Each release comes with [release notes][release-notes] describing the
installation and configuration instructions of the epoch node.

Below is an overview of the installation process for different platforms,
building the package from source, configuration and operation of the epoch
node.

[packages]: https://github.com/aeternity/epoch/releases

## Installation

See the documenation on how to:
* [install the epoch node from the release binary][installation-release-binary]
* [install the epoch node on Windows][installation-windows]
* [install CUDA miner on Ubuntu][installation-cuda-miner]

[installation-release-binary]: /docs/installation.md
[installation-windows]: /docs/installation-windows.md
[installation-cuda-miner]: /docs/cuda-miner.md

## Building from source

See [the documentation on how to build the epoch node from source][building-from-source].

[building-from-source]: /docs/build.md

## Configuration

See [the documentation on how to configure the epoch node][configuration].

[configuration]: /docs/configuration.md

## Running the node

See [the documentation on how to operate the epoch node][operation].

[operation]: /docs/operation.md

## Docker

See [the documentation on how to run the epoch node if you prefer Docker][docker].

[docker]: /docs/docker.md

# Additional resources

* [Threat Model](https://github.com/aeternity/aetmodel/blob/master/ThreatModel.md)
