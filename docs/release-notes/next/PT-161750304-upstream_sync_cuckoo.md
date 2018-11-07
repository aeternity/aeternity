* Changes naming scheme for miner executables. Used to be `lean/mean/cuda<node-bits>`, this is changed to
  `lean/mean/cuds<edge-bits>` to align with upstream cuckoo. An upstream sync is also performed. *NOTE:*
  changes in `epoch.yaml` are necessary (i.e. `lean30 -> lean29`, etc.). Does not affect consensus.
