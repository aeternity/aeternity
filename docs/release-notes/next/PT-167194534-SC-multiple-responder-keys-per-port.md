* Multiple different State Channel responder pubkeys can now share the same listen port
* The `channel_open` and `channel_accept` messages now contain both the initiator and
  responder public keys. This is not backwards compatible for the `noise` protocol.
