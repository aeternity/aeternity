# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.3.0) is a maintenance Lima release.

* Allow all TX types (except `ga_meta_tx`, `paying_for_tx` and `offchain_tx`) in `dry-run`
* Added new metrics to ae.epoch.aemon namespace
    * `block.propagation_time.key`   : Time until key-blocks reached this node in milliseconds
    * `block.propagation_time.micro` : Time until micro-blocks reached this node in milliseconds
    * `block.time_since_prev.key`    : Time between key-blocks in milliseconds
    * `block.time_since_prev.micro`  : Time between micro-blocks in milliseconds
    * `chain.top.difficulty`         : Difficulty of the top block
    * `forks.micro.count`            : Count of observed micro-forks
    * `forks.micro.height`           : Height difference of observed micro-forks
* Increased histogram timespan to 1 hour for some aemon metrics
* Changed the default configuration handling of peers to always include the
  seed nodes (unless explicitly blocked). The idea is to make it harder to
  exclude the seed nodes by mistake.
* Makes some peer pool parameters configurable:
    * `gossiped_peers_count`
    * `select_verified_peer_probability`
    * `max_update_lapse`
    * `standby_times`
    * `max_rejections`
* Sets default `select_verified_peer_probability` to 1.0 to make sure a peer is selected from the verified pool first.
* Sets default `max_update_lapse` to 2592000000 (30 days).

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
