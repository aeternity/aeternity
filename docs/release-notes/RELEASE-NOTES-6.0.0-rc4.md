# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.0.0-rc4) is the fourth Iris release candidate.

It:

* Fixed the default value of the gas limit for the `external` dry-run
  endpoint. It used to have a default of 1 000 000 gas per call and now it is
  6 000 000 per call.

* Set height for `iris` hard fork to happen on block 441444, on 10 June 2021,
  around 9:11am UTC

You can join `testnet`, the hard fork kicked in there at [height 425900](https://github.com/aeternity/aeternity/blob/v6.0.0-rc2/apps/aecore/src/aec_hard_forks.erl#L106).

Please let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
