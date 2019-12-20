# Introduction

This document describes how to configure network monitoring within the node provided by `aemon` application.

## Configuration

By default monitoring is disabled. To turn it on `monitoring.publisher.pubkey` and `monitoring.active = true` need to be setup. To start posting transaction setup `monitoring.publisher.autostart` to `true`.

To post transactions account's public and private key is needed. For online processing only public key needs to be configured. Offline post-processing can be done by filtering transaction by publisher public key.

## Metrics configuration

Monitoring uses statsd backend provided by `apps/aecore/src/aec_metrics.erl`. See metrics `metrics.*` configuration keys in `apps/aeutils/priv/aeternity_config_schema.json`

## Metrics

Each metric uses `ae.epoch.aemon.` prefix.

Name                               | Type      | Description
---------------------------------- | --------- | -----------
`confirmation.delay`               | histogram | Number of keyblock created before signing transaction
`forks.micro.count`                | counter   | Count of observed micro-forks
`forks.micro.height`               | histogram | Height difference of observed micro-forks
`gen_stats.microblocks.total`      | histogram | Number of microblocks in a generation
`gen_stats.tx.monitoring`          | histogram | Number of monitoring transactions in a generation
`gen_stats.tx.total`               | histogram | Number of transactions in a generation
`publisher.balance`                | gauge     | Publisher balance
`publisher.post_tx.max_adjustment` | counter   | Transaction posting error:
`publisher.post_tx.nonce_too_high` | counter   | Transaction posting error:
`publisher.post_tx.nonce_too_low`  | counter   | Transaction posting error:
`publisher.post_tx.success`        | counter   | Successful transaction posts
`publisher.queue.size`             | histogram | Number of transactions posted but not signed on chain
`publisher.queue.ttl_expired`      | histogram | Number of transactions with expired ttl
`block.propagation_time.key`       | histogram | Time until key-blocks reached this node in milliseconds
`block.propagation_time.micro`     | histogram | Time until micro-blocks reached this node in milliseconds
`block.time_since_prev.key`        | histogram | Time between key-blocks in milliseconds
`block.time_since_prev.micro`      | histogram | Time between micro-blocks in milliseconds
`block.tx.total.micro`             | histogram | Number of transactions in a microblock
`block.gas.total.micro`            | histogram | Gas used per microblock
`block.gas.per_tx.micro`           | histogram | Gas used per transaction in a microblock
`block.size.per_tx.micro`          | histogram | Size of transactions in a microblock in bytes
`chain.top.difficulty`             | gauge     | Difficulty of the top block

## How to read metrics

#### `confirmation.delay`
represents network latency. A high number might imply a busy network or unfair leaders.

#### `forks.micro` & `gen_stats.microblocks.total`
`forks.micro` represents the length of microfork. It shows a minimum number observed by monitoring, not an exact one. Based on behaviour observed in mainnet in the first half of 2019, roughly 33% of microblocks transactions are rewritten to the next generation. Use `gen_stats.microblocks.total` as a reference.

#### `gen_stats.microblocks.total` and `gen_stats.tx.{monitoring,total}`
Statistic metrics can be used to measure network saturation

#### `publisher.post_tx.*`
Metrics can be used to monitor mempool's transaction propagation. When `publisher.post_tx.nonce_too_high` is preset you might want to check `mempool.nonce_offset` configuration/

#### `publisher.queue.*`
For further network transaction propagation investigation. All transactions accepted by mempool are tracked by `*.size`. Over time it should correlate with `gen_stats.tx.monitoring`.

`*.ttl_expired` might imply low transaction fee or busy network.
