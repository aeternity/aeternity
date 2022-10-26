## Rosetta implementation for Aeternity

Rosetta is a standardised HTTP API for interacting with the blockchain (https://www.rosetta-api.org/docs/welcome.html)

It comes with its own API compliance test tool rosetta-cli (https://github.com/coinbase/rosetta-cli)

The Aeternity node currently supports a subset of the API endpoints at this time, primarily the read only parts of the specification:

- [x] `/network/list`
- [x] `/network/options`
- [x] `/network/status`
- [x] `/account/balance`
- [ ] `/account/coins` (only required for UTXO blockchains, so not needed for AE)
- [x] `/block`
- [x] `/block/transaction`
- [x] `/mempool`
- [x] `/mempool/transaction` (partially supported - balance events missing)
- [ ] `/construction/combine`
- [ ] `/construction/derive`
- [ ] `/construction/hash`
- [ ] `/construction/metadata`
- [ ] `/construction/parse`
- [ ] `/construction/payloads`
- [ ] `/construction/preprocess`
- [ ] `/construction/submit`

The Rosetta server listens by default on localhost at port 3213. This can be configured in your aeternity.{json,yaml}

## Interacting with the Rosetta API

This directory contains a configuration file and balance bootstrap file for testing against an aeternity mainnet node

The easiest way to query the rosetta endpoint is using the rosetta-cli tool. Some examples:

### Find the balance of an account at the given height

``~/go/bin/rosetta-cli --configuration-file rosetta.cfg view:balance '{"address":"ak_24jcHLTZQfsou7NvomRJ1hKEnjyNqbYSq2Az7DmyrAyUHPq8uR"}' 34453``

### View a block with its balance transfers

``~/go/bin/rosetta-cli --configuration-file rosetta.cfg view:block 142409``

### Check that every block reports balance changes in line with the reported balance on the blockchain

This test is time consuming, expect it to take several days to traverse a local mainnet node.

Note also that this test requires an Aeternity node with garbage collection disabled.

``~/go/bin/rosetta-cli --configuration-file rosetta.cfg check:data``

## Mapping of Aeternity concepts to Rosetta

Aeternity uses bitcoin-ng consensus. At each height an Aeternity node stores a single keyblock and zero or more microblocks. The microblocks contain all the transactions, and the keyblock contains the details of mining. Production of a keyblock also creates internal balance changes for expired name auctions and expired oracle queries, alongside the mining rewards.

For the purposes of Rosetta we consider the combination of a keyblock and all its associated microblocks to be a single 'block'. Microblocks are not visible to Rosetta clients.

Rosetta also expects balance changes to be applied in the same 'block'. This causes some difficulty with the normal Aeternity definition of balance at a height because first a keyblock is produced, then microblocks are added on top of the keyblock at that same height. Normally asking for the balance at a height in an Aeternity system will give the balance at the keyblock, which is before any of the transactions have been applied.

For this reason asking for the balance at a specific height using the Rosetta API will provide the balance at the start of the following keyblock (Aeternity height + 1).