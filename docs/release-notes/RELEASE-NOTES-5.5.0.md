# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.5.0) is a maintenance Lima release.

* Remove HTTP API endpoint `v2/contracts/{hash}/store` due to excessive computational impact.
* State Channel FSM: fixes fee computation according to transaction size.
  Until now close mutual transactions were expecting a fee to be provided by
  the initiating party, or a default value of 20 000 gas would be used instead.
* State Channel FSM: adds support for an optional `gas_price` parameter for
  all on-chain transactions, including the channel create one. This
  `gas_price` is to be used in transaction fee computation: the fee is
  `gas_price * transaction_gas_cost`. The `transaction_gas_cost` is subject
  to a couple of parameters, one of which is the size, adding a bigger fee
  could lead to a bigger transaction and bigger fee expectations. This makes
  the fee computation not trivial and thus this functionality could be
  valuable for State Channel clients.
* Changes which peers are propagated - only the peers from the verified pool are
  in the ping message now. Before there were peers from both verified and
  unverified pools.
* Adds a periodic TCP check of peers from the unverified pool. If the TCP
  connection to a peer is established, it's considered alive and it's moved to
  the verified pool.
* Updates default `max_update_lapse` to 10800000 ms (3 hours) - used to be 30 days.
* Adds garbage collector for removing old account states to free up space on disk.
  By default the GC is disabled, as currently it is experimental feature.
  If it is enabled, the default (configurable) interval is 50000 blocks (roughly once in 3 months).
  Explicit configuration would look like:
    ```
        chain:
            ...
            garbage_collection:
                enabled: true
                interval: 50000
                history: 500
    ```


Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
