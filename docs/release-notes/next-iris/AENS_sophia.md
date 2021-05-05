* Added `AENS.update` to FATE VM
* Added `AENS.lookup` and `Oracle.expiry` lookup functions to FATE VM
* Fixed bug regarding TTL of preclaims in FATE VM - it was incorrectly always
  set to 0, from `VM_FATE_SOPHIA_2` it has the correct value.
* Fixed bug in `AENS.resolve` in FATE VM - for invalid names `VM_FATE_SOPHIA_1`
  will crash. From `VM_FATE_SOPHIA_2` it will not crash, rather return `None`.
