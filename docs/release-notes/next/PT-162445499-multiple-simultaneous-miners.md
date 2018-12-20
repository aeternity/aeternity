- Changes cuckoo miner config and processing to allow multiple simultaneous miners.
  This impacts mining configuration:
  * changes `mining` > `cuckoo` > `miner` param to `mining` > `cuckoo` > `miners` and makes `mining` > `cuckoo` > `miner` param deprecated
  * moves `mining` > `cuckoo` > `miner` > `edge_bits` param to `mining` > `cuckoo` section
