Ceres is the 5th protocol upgrade for aeternity; following the initial protocol Roma, we have then seen Minerva, Fortuna, Lima, and Iris. Ceres is the first protocol upgrade not part of the initial update schedule and it contains mainly smaller improvements and enhancements.

# Changes summarized
Micro-block gas limit calculation changed: Gas limit was computed from a transaction's max gas value, now it is computed from the actually used gas.

* Contracts can be called by name: Just like it is possible to spend to an AENS name, it is now possible to call a function for a name that points to a contract.

    * Generalized Accounts:

    * GAMetaTx parameters (fee and gas_price) are now part of the transaction hash and thus not changeable by a rouge miner.
GAAttachTx can only be used by a fresh account.

* AENS:
  * Adjusted auction parameters - auctions are generally made shorter.
  * Adding raw data pointers.
  * Adding wildcard delegation signature, delegating control for all current, and future, names for an account to a particular contract.
  * PreClaimTx is made optional.
* FATE:
  * Delegation signatures have been made more structured.
  * Support for dynamic-sized binary arrays (type bytes()) is added.
  * Built-in bitwise operations have been added.
  * Crypto.poseidon - a SNARK-friendly hash function is added.
* Utility functions Chain.network_id, Address.to_bytes, and Int.mulmod are added.
* AENSv2 is introduced to support the raw data pointers.
* BRI - the Block Reward Initiative has been made configurable. The BRI was introduced in Fortuna and hasn’t changed since. In Ceres it has been made a proper part of the configuration, and the mechanism has been updated to allow changing it as part of a protocol upgrade. Values for mainnet and testnet remain hardcoded.

# Changes detailed

These changes are provided by the PR's:
https://github.com/aeternity/aeternity/pulls?q=is%3Apr+label%3Abreaking%2Fconsensus+

## Micro-block gas limit

Instead of using max allowed gas when packing micro-blocks we use actual used gas. This allows for more transactions to be packed into a block, increasing throughput and also stopping (accidental) denial of resources when overestimating contract calls, etc. The downside of this is that the transaction has to be applied before its gas usage is used. This puts a little bit of extra burden on the block producer, but, thankfully, block verification is not computationally more expensive.

## Contracts called by name
Contracts can be called by name. Just like it is possible to spend to an AENS name, it is now possible to call a function for a name that points to a contract. The key contract_pubkey of the name is resolved while running the transaction. Technically, this has consequences for the contract call structure
(aect_call) that gets a new field with the contract call identifier. To be able to find the call result without doing name resolution calls are stored relative to the id rather than the contract pubkey.
NOTE: Names can change at any time (frontrunning is possible!) so only call contracts by name when you trust the owner of the name!

## Generalized Accounts - hardening GAMetaTx

Include fields fee and gas_price in GAMetaTx when computing the TX-hash of the inner transaction. This way a malicious miner can't change them before inserting the transaction in a macroblock.

## Generalized Accounts - limit GAAttachTx

The transaction logic for GAAttachTx is changed, such that, it is only possible to attach authorization code to a fresh account. An account is fresh as long as it hasn't signed any transactions - effectively, this means that GAAttachTx is only allowed with nonce = 1.
The rationale behind this is to avoid the case where an active account is lured into signing a GAAttachTx, and in practice giving the account keys away.

## AENS - Adjusted auction parameters
The AENS auction mechanism is adjusted. Firstly, we make auctions initially shorter, and, secondly, new bids only extend auctions with a short period (and it is only extended if the new bid comes near the end of the auction, i.e. within the extension period).
Names longer than 12 characters, like before, have no auction. Names between 9 and 12 characters have an auction length of 480 generations, approximately 24 hours. Names between 5 and 8 characters have an auction length of 960 generations, approximately 48 hours. Names shorter than 5 characters have an
auction length of 2400 generations, approximately 5 days. The extension period is 120 generations, approximately 6 hours.
## AENS - Raw data pointers
We introduce a new type of pointer for AENS names. The raw data pointer/value is an arbitrary-sized byte array (max 1024 bytes). And, unchanged, a name can have a maximum of 32 key-pointer pairs. FATE VM and Sophia are updated to allow full access to the new pointer type (See
https://github.com/aeternity/aesophia/blob/master/docs/sophia_stdlib.md#aensv2).
## AENS - Wildcard delegation signatures
Add generic/wildcard AENS delegation signatures. I.e. instead of delegating authority for a contract to operate with a specific name (name hash), by signing just the network id, public key, and contract address, you can delegate the authority for a contract to handle all your names with one signature. See Issue #4080 for details.
BEWARE: This gives the contract authority to handle all current and future names on your behalf, so it should be used with extreme care and only for well-known (and well-understood!!) contracts.
## AENS - Preclaim is optional
Since the introduction of auctions, the front-running protection offered by the
'PreClaim -> Claim' flow is no longer as important. To simplify name
registrations (or the start of a name auction) we now allow NameClaimTx without
a preceeding NamePreclaimTx. In this case, we set the NameSalt to 0.
## FATE - New operations
* bitwise operations on integers (band, bor, bxor, bnot, << and >>)
* Address.to_bytes - converting an address to its binary representation
* Int.mulmod - a combined multiplication and modulus operation for efficiency
* Crypto.poseidon - a ZK/SNARK friendly hash function
* Introduce arbitrary sized binary arrays (type bytes()); adding Bytes.split_any,
 Bytes.to_fixed_size, Bytes.to_any_size, Bytes.size, String.to_bytes,
 and Int.to_bytes; and adjust Bytes.concat, and Crypto.verify_sig to allow both
 fixed and arbitrary sized byte arrays.
* Chain.network\_id - the network id of the chain
### FATE - Structured delegation signatures
Delegation signatures have been made more structured, and their serialization is part of the protocol description (and their implementation is part of aeserialization). This means that the data for delegation signatures no longer risks overlapping with transactions, and it should be clear what is
signed. This is discussed in #4177 and #4178.
