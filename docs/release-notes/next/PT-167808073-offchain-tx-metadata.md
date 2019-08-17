* Metadata objects (of type binary) can now be added to an offchain update transfer request.
  These objects serve as comments and are not part of the signed transaction, nor do they
  affect the offchain state trees. This is an incompatible change in the serialization of offchain
  updates, but old offchain updates can still be deserialized.
