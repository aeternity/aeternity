# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.5.0) is a maintenance Iris release.

It:

* Fixes serialisation if `int-as-string` is enabled: numbers in lists gets
  converted to strings, `dry_run` endpoint accepts balance as string.

* Introduces a State Channel option, `msg_forwarding :: boolean()`, which enables forwarding of generic messages based on the `from` and `to` headers.
  Without `msg_forwarding: true`, generic messages are discarded if the `from` field doesn't match the peer pubkey and the `to` field doesn't match the
  receiving FSM's pubkey (the `from` value is implicit and doesn't need to be provided by the sender). With `msg_forwarding` enabled, the sending client
  can optionally provide either a remote `from` or a remote `to` (currently not both at the same time). The receiving FSM will add a `notice` field to
  the report to the client: the notice value will be one of `direct`, `please_forward` or `forwarded`, allowing the client to act accordingly.
  Note that in order to forward a message, the client needs to have a State Channel open to the intended recipient, and copy the message to that
  channel. An example can be found in the `apps/channels/test/aesc_htlc_SUITE`.

* Introduces a new HTTP endpoint for fetching peer pool stats: connected
  (inbound and outbound), available for a new connection (verified and not yet
  verified) and blocked peers. This is part of the `node-operator` group that
  is bound to the internal interface and disabled by default. This new
  endpoint is intended for the node operator to be monitoring its node's
  connectivity.

* Introduces a standalone binary artifact for supported platforms. Experimental.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

