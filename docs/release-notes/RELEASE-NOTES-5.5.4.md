# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.5.4) is a maintenance Lima release.

It:

* Bugfix: Exposes the State Channel force progress transaction call result to HTTP API

* Bugfix: Under specific circumstances the store of a contract could be damaged
    by updating values in the wrong place. A transaction experiencing this would
    not be valid for peers which are not affected by this bug. The fix ensures the store
    update is correct.

    This bug affects nodes running v5.4.x and v5.5.x, therefore we advise to
    upgrade these nodes to prevent any issues from occuring.

* Fixes a State Channel WebSocket API issue: if a user provided a fee, it was
  ignored

* Fixes a State Channel WebSocket API issue: when participants try closing a
  channel with insufficient channel balance to cover the fee, a proper error
  is being sent.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

