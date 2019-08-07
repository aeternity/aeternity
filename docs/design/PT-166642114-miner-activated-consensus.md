# Miner Signalled Consensus Upgrade: Design

This document describes the technical specifications for the miner signalled consensus upgrade whose [functional specifications are available][fspecs].

[fspecs]: https://github.com/aeternity/protocol/blob/1ccb7fb099f7da9ba682b0039d37790050499ed1/consensus/miner_signalled_consensus.md

## Technical specifications

### Behaviour

#### User configuration

The user may specify via the user configuration file the greatest consensus protocol version in such user configuration file to be activated depending on miner signalling.
At node startup, the node must parse, validate and load such configuration.
As miner signalling uses block header field `info`, the [current (i.e. old) consensus protocol version C1][fspecs] must be strictly greater than Minerva.

#### Blocks

##### Blocks connected to genesis

The `aec_conductor` process, that is the entry point for all changes to the chain storage, enforces that each block being inserted is connected to genesis.
By exploiting such current design, the expected consensus protocol version of a block can be computed whenever:
- An untrusted block is meant to be able to be inserted in the storage.
  This includes the cases of:
  - Sync worker process intending to add one block to the storage.
  - Gossip.
  - User API.
- A trusted block is meant to be able to be inserted in the storage.
  These blocks are not validated.
  This includes the cases of:
  - Locally mined or signed block.
  - Mined block submitted by Stratum mining pool server (if node configured as Stratum server).
- A block candidate (eventually leading to a trusted block) is prepared.
  This includes the cases of key block and micro block.

The check on such expected consensus protocol version
whenever an untrusted block is meant to be able to be inserted in the storage - and any further deferred validation that that entails -
can be performed outside of the context of the `aec_conductor` process.
This follows the current design of the `aec_conductor` process,
that relies on [most block validation having been performed in the caller's context](https://github.com/aeternity/aeternity/blob/d877a856648bd69cb1b473efa9c6149725d8d74c/apps/aecore/src/aec_conductor.erl#L1072-L1074):
> Block validation is performed in the caller's context for
> external (gossip/sync) blocks and we trust the ones we
> produce ourselves.

(Blocks from the internal user API are considered equivalent to blocks received via gossip, hence untrusted and validated.)

Keeping these checkes outside of the context of the `aec_conductor` process is relevant
in order to prevent passing to the `aec_conductor` process too many invalid blocks
(either for chain forks caused by the change in consensus protocol version, or for malicious activity).
Specifically, the contexts in which the consensus version must be checked beforehand outside of the context of the `aec_conductor` process are:
- The sync worker process.
- The peer connection / gossip process.
- The HTTP user API worker.

For trusted blocks, the conductor must instruct the generation of the block candidate with the correct consensus protocol version.

###### Asynchronous computation of expected consensus protocol version

In order to prevent long and potentially duplicated computational efforts - requiring multiple lookups into the chain storage -
the computation of the outcome of the miner signalling should be asynchronous and lazy.

Whenever a block at height preceding the signalling block interval end is inserted in the chain storage,
the `aec_conductor` spawns - and keeps track of - a worker responsible for
computing the outcome of the miner signalling - using non-transactional chain storage access -
and storing such outcome (true / false) in an auxiliary table.

In order to play well with non-transactional chain storage read operations,
the outcome should be written (shortly before the worker terminates)
with a storage access that enables non-transactional readers to see the outcome of the write
immediately after the synchronous write call finishes from the point of view of the worker.

Callers requiring the signalling outcome for a certain block
shall enquiry the chain storage and, if no outcome yet,
ensure that there is a worker alive for that then:
- For sync worker process,
  yield (`take_a_break`).
- For user API worker process,
  terminate returning 503 with reason "Computing miner signalling outcome".
- For peer connection (gossip),
  ignore logging info message.
- For locally mined block (hence context of `aec_conductor` process),
  ignore logging error message (as it should not have happened).
- For mined block submitted by Stratum server (hence context of `aec_conductor` process),
  ignore logging error message (as it should not have happened).

##### Blocks not necessarily connected to genesis

The current design of syncing receives blocks out of order.
Therefore the validation of the consensus protocol version in each block at that stage needs to be relaxed.
Any deferred validation must be performed on each single block when meant be inserted in the chain storage,
as at that stage such block is meant to be connected to genesis.

##### Blocks persisted in the database with a now-invalid consensus protocol

**TODO Handling of blocks read from database after user configuration makes consensus upgrade unconditional.**

#### Transactions

##### Mempool

###### Insertion

When configured for miner signalling, then node relaxes checks on transactions
so to accept both transactions valid under the current consensus protocol or under the proposed new consensus protocol.

###### Deletion

When configured for miner signalling,
after - on the best valid chain - the proposed fork height is reached and the consensus protocol version is decided,
the node will not re-check transactions for the now-invalid protocol.

After having configured the node for miner signalling,
if afterwards the user configures a fixed consensus protocol at a certain height and restarts the node
the node will not re-check transactions for the now-invalid protocol.

##### State channel client finite state machine

**TODO (The state channel client finite state machine handles transactions assuming consensus protocol version.)**

**TODO Handling of transactions read from database after user configuration makes consensus upgrade unconditional.**

##### User API

**TODO The user API returns transactions to the user. Mark those APIs as deprecated and remove those? Let user specify consensus protocol?**

##### Other reads

**TODO Handling of transactions read from database after user configuration makes consensus upgrade unconditional.**

### Data structures

#### User configuration

User configuration for accommodating the [defined parameters][fspecs].
**TODO Detail format and constraints. (See [comment Pivotal Tracker ticket](https://www.pivotaltracker.com/story/show/166642114/comments/205266011) and following comments.)**

#### Auxiliary table for asynchronous computation of outcome of signalling

Table whose name is computed based on the set of [user configured parameters][fspecs] - including current consensus protocol version C1, and excluding the height H1 at which it was first effective.
Key is hash of key block at height preceding that at which signalling block interval ends (so [HE][fspecs] minus 1).
Value is boolean (true / false), expressing whether the proposed new consensus protocol is meant to be enabled at the proposed height.

## Pending clarifications

Review/confirm/refine/revise the value in the key block header field info being part of the validation rules of the new consensus protocol on the "signalling" block interval.

Change across the node the assumption that consensus protocol version can be inferred simply from the height, and rather:
- If block expected to be connected to genesis, perform non-transactional reads to the chain storage.
- Otherwise, relax check at that stage though ensuring check performed at a later stage.
  This involves checking that no denial-of-service attack vector is opened.

Cleanup of auxiliary table re signalling outcome.
In order to prevent irrelevant tables to build up following multiple node restart with multiple distinct miner signalling configurations,
shall node at startup, if a miner signalling is configured, delete all auxiliaty tables with name associated with different miner signalling parameter?
Doing so would prevent irrelevant tables (and - potentially - content) to build up though would open the door for inconsistent chain storage,
requiring the node to re-trigger the asynchronous computation of the miner signalling outcome.
