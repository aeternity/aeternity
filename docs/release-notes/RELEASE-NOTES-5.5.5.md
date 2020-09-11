# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.5.5) is a maintenance Lima release.

It:

* Add helper function `aeu_log:set_console_log_level/1`. Use it to set the level
    to `warning` to remove info logs from the system's console.

* The event and message history kept by the State Channel FSM can now be fetched using the WebSocket API method channels.history.fetch

* State Channel FSM bugfix: when one party asks its FSM to create an unilateral transaction
  (ex. close solo) and while the FSM is waiting for it to be confirmed, the
  other party could create an off-chain update that would cause a conflict on
  both ends. This could deny the former party the possibility of a dispute.
  Fixed

* Enhances FSM behaviour: when the initiator is offline, allows the responder
  to stay online waiting for it even if the timeout timer is reached.

* Added CORS headers for error responses

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

