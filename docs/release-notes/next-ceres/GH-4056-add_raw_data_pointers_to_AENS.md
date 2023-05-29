* Introduce a new type of pointer for AENS names. The raw data pointer/value is
  an arbitrary sized byte array (max 1024 bytes). And since before there is a
  maximum of 32 key-pointer pairs. FATE VM and Sophia are updated to allow full
  access to the new pointer type (See
  https://github.com/aeternity/aesophia/blob/master/docs/sophia_stdlib.md#aensv2).
