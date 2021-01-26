# Fork resistance in Aeternity nodes

The Aeternity consensus and sync protocols resolve forks according to the specification
in [the aeternity Bitcoin-ng protocol](https://github.com/aeternity/protocol/blob/master/consensus/bitcoin-ng.md). This means that 51% attacks are still possible, just as with other Proof-of-Work chains.

The Aeternity node implementation offers a few ways to withstand 51% attacks.

## Fork resistance via gossip

The configuration variable

```yaml
sync:
    gossip_allowed_height_from_top : <height>
```
limits the height difference allowed for incoming blocks via gossip. This variable has a
hard-coded default of `5`, but can be changed via the `aeternity_config.[yaml|json]`
file (see [the configuration instructions](configuration.md)).
Any keyblock received via gossip will be rejected if its height is more than this
value below the current top.

## Fork resistance via sync

The normal way to introduce a long competing fork would be via the sync protocol. That is,
a node goes off-line and mines a long chain, then reconnects to the network and syncs
against other nodes: if the competing fork has a higher difficulty, it will supersede the
chain on the network.

The following configuration variable,

```yaml
sync:
    sync_allowed_height_from_top: <height>
```
will instruct the node to reject blocks received via sync whose height is more than `<height>`
blocks below the current top. The default value is `100`. A value of `0` disables the
protection. In practice, this should mean that once a transaction has at least `<height>`
keyblocks on top of it, the nodes will resist any competing fork trying to evict it.

The fork resistance is activated once the node has synced with at least one other node.
It is possible to enable fork resistance immediately, using the following setting:

```yaml
sync:
    resist_forks_from_start: true`
```

Note that configuration variables can be set both via the config file and via OS
environment variables. This means that it's possible to instruct the node to resist
forks at a given node start in the following way:

```
AE__resist__forks__from__start=true bin/aeternity start
```
(see [the configuration documentation](configuration.md#configuration-from-the-command-line-or-scripts))

### Discussion

When choosing a suitable fork resistance depth, it is important to consider some tradeoffs.
Blockchains naturally rely on 'optimistic concurrency', that is, it is entirely possible
that different nodes manage to produce keyblock candidates at roughly the same time.
This means that keyblock forking can occur occasionally. The likelihood that it will happen
repeatedly is extremely low, so such forks should resolve quickly, usually with the following
keyblock.

Note that even with very long competing forks, the chain will converge. Absent malice and unless
the `TTL` has been set very low for some transactions, transactions evicted for being on a
"losing fork" will be returned to the mempool and find their way back onto the main chain.
The problem is when this behavior is exploited to evict specific high-value transactions
(so-called "rollback", or "double-spend" attacks).

Even if all transactions get returned to the chain, miners who have mined several blocks
on a losing fork may have reason to feel cheated, as they lose their mining rewards.

Network latency and network failures may increase the frequency and length of naturally
competing forks, and setting the fork resistance too low might increase the risk of
chain splits, where different nodes end up on different forks and cannot resolve the
situation without manual intervention.
