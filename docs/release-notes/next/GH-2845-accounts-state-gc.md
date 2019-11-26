* Adds garbage collector for removing old account states to free up space on disk.
  By default the GC is disabled, and does the cleanup every 50000 blocks.

  The cleanup and swap of nodes can not be done transactionally.
  If user stops the node during the GC `swap_nodes` phase and encounters a `hash_not_found` error,
  a full resync is necessary to recover the node.
