# Introduction

This document describes how garbage collection works in the Aeternity node, and how to
use it.

## What is garbage collection?

Blockchains act as "append-only" repositories, i.e. you can only add data, not modify it.
At any given block height, the "state" of the blockchain is the set of current values
of accounts, contract and oracle states, registered names and ongoing name auctions and
active state channels. For any state object, it is possible to inspect its history by
fetching its value relative to previous block heights. The "root hash" of a block header
identifies a slice of the blockchain state trees representing all the live state and values
that were current at that point in time.

The drawback of this is of course that the size of the blockchain keeps growing. Ultimately,
it becomes impractical to keep the entire chain on a local computer.

Garbage collection is a way to "prune" the history of states, that is we keep the state of
a number of blocks beneath the top, but delete the ones beneath that. Importantly, all the
latest values of all accessible state is always kept. What is deleted is the state that
has been overwritten at some point.

## Configuration

Garbage collection can be customized with the following configuration options:

### `chain:garbage_collection:enabled`

**type: `boolean`**
**default: `false`**

This is the master switch, controlling whether or not garbage collection is active.

### `chain:garbage_collection:during_sync`

**type: `boolean`**
**default: `false`**

If `true`, garbage collection will not wait for chain sync to complete.
Garbage-collecting during chain sync will slow down the sync process - exactly how much
depends on the settings, but around 30 % is a reasonable expectation. The upside is that
the on-disk footprint stays small.

Note that the node cannot be sure when sync is fully completed. Garbage collection will
wait (if `during_sync: false`) for the first indication that sync has completed
_given the information available_. New information may arrive when other peers are
found, but the garbage collector will not go back to waiting, once activated.

### `chain:garbage_collection:minimum_height`

**type: `integer`** (non-negative)
**default: `0`**

If set to a value `> 0`, garbage collection will not kick in until this height is reached.
This could be used to speed up sync (when used together with `from_start: true`),
since chain sync can proceed at full speed until the given height is reached.

### `chain:garbage_collection:history`

**type: `integer`** (minimum: `50`)
**default: `15000`** (ca 31 days)

Determines how much history to keep. The unit is number of key blocks.

### `chain:garbage_collection:trees`

**type: `array`**
**default: _all the trees_**

Makes it possible to garbage-collect only a subset of the state trees.
This would be touched mainly if there is a requirement to actually keep the
history of a given state (e.g. `accounts`), but still reduce the space requirement
somewhat. It could also possibly be a way to reduce the overhead of garbage collection
(also getting less space reduction).

**Example: `["accounts", "ns"]` (the two largest state trees)

The array may not be empty. An empty array would mean that nothing is garbage-collected.
This effect is better achieved using `enabled: false`.

### `regulators:gc_scan`

**type: `object`**

This group allows for configuration of the load regulation of the garbage-collection scans.
You should not touch this unless you are knowledgeable of the node internals and willing
to experiment. The way garbage collection works is that sets of A/B database table pairs
are switched at a given height, and a scan is started for each tree, ensuring that all
reachable state is promoted into the current "A" table. Once the scan has been completed,
AND the `history` depth has been reached since the last switch, a clear-and-switch operation
can be performed. This means that there is normally ample time to perform the sweeps, and
the load regulation parameters can be set quite low.

The situation is a little different during sync, as the `history` depth is likely to be
reached very quickly. It is therefore possible that the load regulation becomes the factor
determining how frequently data can be cleared.

It is important to know that file system IO capacity can easily become the bottleneck,
and trying to run faster, can lead to write stalls and internal timeouts.

The unit of regulation is a "chunk" of `100` state tree object lookups. That is, the
scanner will touch 100 trie nodes, then pause until the regulator gives permission to
continue. Each tree has its own scanner process, running concurrently with the others.

#### `regulators:gc_scan:counter`

**type: `integer`** (minimum: `0` - meaning no counter regulation)
**default: `1`**

This controls how many scanner chunks can be actively processed at the same time.
Increasing this value may not increase performance. It could even have the opposite effect.

#### `regulators:gc_scan:rate`

**type: `integer`** (minimum: `0` - meaning no counter regulation)
**default: `50`**

This controls how many chunks per second are allowed to run.
Increasing this value may not increase performance, but lowering it, should lower
the overhead of garbage collection, possibly at the cost of effective space reduction.
