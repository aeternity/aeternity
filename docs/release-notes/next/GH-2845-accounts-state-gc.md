* Adds garbage collector for removing old account states to free up space on disk.
  By default the GC is disabled, as currently it is experimental feature.

  If it is enabled, the default (configurable) interval is 50000 blocks (roughly once in 3 months).

  Explicit configuration would look like:

```
    chain:
        ...
        garbage_collection:
            enabled: true
            interval: 50000
            history: 500
```

  The cleanup and swap of nodes can not be done atomically.
  If user stops the node during the GC `swap_nodes` phase and encounters a `hash_not_found` error,
  a full resync is necessary to recover the node.
