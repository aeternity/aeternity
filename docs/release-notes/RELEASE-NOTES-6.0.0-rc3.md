# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.0.0-rc3) is the third Iris release candidate.

It:

* Introduced a new HTTP endpoint: `/dry-run`. It is part of the `external`
  interface and should be prefered over the existing `debug` endpoint. It
  comes with some protections for the node: all transactions/calls provided
  are limited to a total amount of `gas` that they can consume. There is a new
  setting in the config where the node operator can change this according to
  their needs, the default value is 6 000 000 gas. The new endpoint is
  disabled by default and can be enabled via the new API group `dry-run`.

Please test the **iris** protocol by activating it in the configuration file as below:

```yaml
  chain:
    hard_forks:
      "1": 0
      "5": 1

 fork_management:
    network_id: "my_test_iris"
```

You can also join `testnet`, the hard fork kicked in there at [height 425900](https://github.com/aeternity/aeternity/blob/v6.0.0-rc2/apps/aecore/src/aec_hard_forks.erl#L106).

Please let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
