# About this release

Second **Lima** release candidate.

Changelog:

* Remove the `rename_db` migration command used to migrate Roma databases.
* Remove the custom docker entrypoint.
  To run the docker image with extra arguments the default command should be used as well, e.g.:
  `docker run aeternity/aeternity bin/aeternity console -noinput -network_id ae_uat` see the corresponding docs for details.
cat: docs/release-notes/next-lima/PT-168132312-new-name-hash: No such file or directory
* No error messages in contract call objects - they will end up under consensus.
  Run your contract in `dry-run` to get the detailed error message.
* Name preclaims are no longer allowed to use `0` as `salt` value
* New name hash computation
* New governance function determining if a name is subject to direct claim or auction
* New governance function determining initial price of a name
* New name auction mechanism using subsequent claim transactions with `salt` equal to `0`
* State Channels: Changed configuration option name `ws_handlers` to `sc_ws_handlers` which
  correctly implies that the limit applies to SC websocket connections.
* State Channels: It is now possible to abort a signing request by replying with an error code.
* State Channels: The FSMs now stay alive until a Close Mutual (shutdown) has been confirmed on-chain
  It is also possible to abort/reject a Close Mutual by rejecting the signing request.
  See https://github.com/aeternity/protocol/blob/master/node/api/channel_ws_api.md#signing-error-replies
* State Channels: On-chain tx monitoring was broken after leave/reestablish. This has been fixed.
* State Channels: Enhances the FSM with the optional pinned environment for execution
  off-chain transactions. This is to improve reaching off-chain consensus as
  both parties share a common view of what is considered to be a fork-safe
  block hash.
* State Channels: Introduces on-disk state cache encryption
    * When a user leaves a state channel the off-chain state will be persisted on disk and protected 
      with a password. The password is provided by the user when opening a channel using the `state_password` 
      parameter.
    * When re-establishing the channel the same password **MUST** be provided, otherwise 
      the operation will fail with error code `invalid_password`.
    * The password **MUST** be at least 6 characters long. Providing a shorter password 
      will fail with error code `invalid_password`.
    * The password may be changed anytime by the user through the websocket connection after the channel 
      has been opened. Please consult the documentation for more details. This operation is only allowed 
      when the channel is established. If the user has left the channel, the channel must be re-established
      first before changing the password.
    * Until the lima fork the password will be optional. By not providing a password a default value is used:
      `correct horse battery staple`. After the Lima fork the password will become mandatory. Pre-Lima off-chain states 
      will be encrypted with the default password.
    * Because the password used for encrypting the persisted state cache is mandatory after the Lima fork, 
      state channels which were opened before Lima use the default password. 
      Not providing the password in the websocket connection will result in a `missing_field` error.
    * Keep in mind that an adversary with direct RAM access to the node may still steal the off-chain state.
      This change only protects the state against an adversary with direct disk access.

The node is able to start from a database produced by v4.* releases. For the rest, this release is not backward compatible with v4.* releases.

Please join the mainnet by following the instructions in the documentation below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
