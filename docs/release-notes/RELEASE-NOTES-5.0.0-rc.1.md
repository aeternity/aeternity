# About this release

First **Lima** release candidate.

Changelog:

* We now have FATE. (The initial version of FATE is released as VM version `0x05`).
* The [```channel_close_mutual```](https://github.com/aeternity/protocol/blob/master/channels/ON-CHAIN.md#channel_close_mutual)
  transaction can now be performed while the channel is not active on-chain (The channel solo closing sequence had been initiated and the channel is not yet closed),
  this is a consensus breaking change available after the Lima fork.
* The state channel FSM after the Lima fork accepts a shutdown message while the channel is being closed unilaterally.
* Introduces AEVM version `0x06`, available from Lima consensus.
  This is a fine-tuned version of the AEVM version `0x04` introduced in the Fortuna consensus.
* Extends transaction signing - also allow signatures of the transaction hash.
  The transaction serialization is potentially long, so signing the
  transaction hash means less cryptographic work.
* Obsoletes the old State Channel off-chain transaction type which contained
  the list of updates that produced the latest `state_hash`. The new
  transaction type is available since Fortuna hard fork and the FSM is
  producing such transactions ever since. This detaches the off-chain protocol
  from the on-chain one and allows development of unique off-chain protocols
  that don't need their updates to be serializable on-chain.
* For existing state channels where the latest co-authenticated state is an
  off-chain transaction based on the old version, it is suggested to make a
  new co-authenticated transaction which is using the new serialization.  For
  currently existing state channels which latest co-authenticated state is an
  old version off-chain transaction is suggested to make a new
  co-authenticated transaction that is using the new serialization. If the
  other party refuses to authenticate a new round or is simply missing, one
  can use the solo closing sequence instead.
* Metadata objects (of type binary) can now be added to an offchain update transfer request.
  These objects serve as comments and are not part of the signed transaction, nor do they
  affect the offchain state trees. This is an incompatible change in the serialization of offchain
  updates, but old offchain updates can still be deserialized. When creating a channel with a party
  running an older node version, add `version_offchain_update=1` to the channel params.

The node is able to start from a database produced by v4.* releases. For the rest, this release is not backward compatible with v4.* releases.

Please join the mainnet by following the instructions in the documentation below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
