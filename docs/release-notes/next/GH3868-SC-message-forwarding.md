* Introduces a State Channel option, `msg_forwarding :: boolean()`, which enables forwarding of generic messages based on the `from` and `to` headers.
  Without `msg_forwarding: true`, generic messages are discarded if the `from` field doesn't match the peer pubkey and the `to` field doesn't match the
  receiving FSM's pubkey (the `from` value is implicit and doesn't need to be provided by the sender). With `msg_forwarding` enabled, the sending client
  can optionally provide either a remote `from` or a remote `to` (currently not both at the same time). The receiving FSM will add a `notice` field to
  the report to the client: the notice value will be one of `direct`, `please_forward` or `forwarded`, allowing the client to act accordingly.
  Note that in order to forward a message, the client needs to have a State Channel open to the intended recipient, and copy the message to that
  channel. An example can be found in the `apps/channels/test/aesc_htlc_SUITE`.
