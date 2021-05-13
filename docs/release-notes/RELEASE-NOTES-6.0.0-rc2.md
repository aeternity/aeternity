# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.0.0-rc2) is the second Iris release candidate.

It:

* Fixed a HTTP API issue: in `/v2/` when fetching oracle queries, the client
  can specify if they want currently `open`, already `closed` or simply `all`
  queries for that oracle. If no parameter was provided, the endpoint crashed.
  Now if nothing is specified, `all` queries are returned as this is the
  default behaviour for `/v3/`.

* Fixed a HTTP API issue: in `/v3/` when fetching a transaction by hash, if
  the transaction is `ga_attach_tx`, the API crashed.

* Fixed a HTTP API issue: in `/v3/` when fetching a `channel_create_tx`, the
  API crashed.

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
