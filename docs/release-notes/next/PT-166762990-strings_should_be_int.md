* Fix incorrectly typed fields in JSON responses for `/transactions/{hash}/info` endpoint. `abi_version` and
  `vm_version` should be integer values, but for some transactions they were (hex-)strings.
