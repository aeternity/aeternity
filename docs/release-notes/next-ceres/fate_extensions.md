* Adds new operations to the FATE VM (version 3)
  - bitwise operations on integers (`band`, `bor`, `bxor`, `bnot`, `<<` and `>>`)
  - Address.to\_bytes - converting an address to its binary representation
  - Int.mulmod - a combined multiplication and modulus operation for efficiency
  - Crypto.poseidon - a ZK/SNARK friendly hash function
  - Introduce arbitrary sized binary arrays (type `bytes()`); adding `Bytes.split_any`,
  `Bytes.to_fixed_size`, `Bytes.to_any_size`, `Bytes.size`, `String.to_bytes`,
  and `Int.to_bytes`; and adjust `Bytes.concat` to allow both fixed and arbitrary
  sized byte arrays.
  - Chain.network\_id - the network id of the chain
