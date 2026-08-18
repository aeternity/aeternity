* Fixes the forward generation lookup for a key block that is a fork sibling at
  the current top height. Such a block was returned paired with the micro blocks
  of the main chain - a generation that never existed on any chain, and one that
  is indistinguishable from a genuine one to a client. It is now rejected, which
  is what the same lookup already did for fork blocks below the top height:
  `GET /v3/generations/hash/{hash}` answers `400 Hash not on main chain` instead
  of `200`, and the peer-to-peer `get_generation` request answers
  `block_not_found`. Generations of main chain key blocks, lookups by height,
  and backward generation lookups (which are defined for fork blocks too) are
  all unaffected.
