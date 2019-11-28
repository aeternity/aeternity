# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.2.0) is a maintenance Lima release
focused on stability:

* SC clients will now receive a token which will be required for reestablishing a channel or
reconnecting to an existing FSM, the token must be persisted and kept secret by the client.
* Removes the user provided password from the state channel API
* Removes the special channel reconnect transaction -
reestablishing a channel may now result in reconnecting to an existing FSM
* This release ships with backwards incompatible changes to the client WS API. 
Make sure a compatible SDK version is used when using state channels. Existing users of SC should
close their pre v5.2.0 channels and reopen them after v5.2.0. After v5.2.0 reconnection requests won't
require signing a transaction.
* Add database write retry logic, to prevent immediate failure during temporary
  disk issues. The number of retries can be configured in the user configuration.
* Avoid JSON-encoding errors by sending oracle query/response format as `contract_bytearray` also
  for `ABI_FATE_SOPHIA_1` in `/oracles/{pubkey}` HTTP API endpoint.
* Adds periodical log of number of connected peers
* Adds the number of connected peers (inbound and outbound) to HTTP /status endpoint

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
