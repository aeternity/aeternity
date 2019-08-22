* Metadata objects (of type binary) can now be added to an offchain update transfer request.
  These objects serve as comments and are not part of the signed transaction, nor do they
  affect the offchain state trees. This is an incompatible change in the serialization of offchain
  updates, but old offchain updates can still be deserialized. When creating a channel with a party
  running an older node version, add `version_offchain_update=1` to the channel params.
