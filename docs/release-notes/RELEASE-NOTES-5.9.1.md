# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.9.1) is a maintenance Lima release.

It:
* Upgrades the swagger definition to OAS3.

* Introduces a new query param in relevant HTTP APIs. It is called
  `int-to-str` and specifies that all integers in the response shall be
  represented as strings instead. This will allow SDKs to pick their
  preference for the data representation and will help them solve precision
  issues. This applies only for v3 API that supports `oas3` and is not
  supported by the `swagger v2` API

* Allows passing strings instead of integers in `post` requests for `debug`
  APIs that are convenience for testing and developing SDKs.

* Introduces a second HTTP specification. This is to provide the new `OAS3`
  API under `/v3/` while we keep the `Swagger v2` API unchanged under `/v2/`.
  This aims to provide a smooth transition from old specification to the new
  one. The new API is to be finalized with `iris` hardfork, please do not
  depend on it yet.

* Expands the `/api` endpoint with an option: while simply calling `/api`
  keeps the old behaviour, calling `/api?oas3` will provide the new `OAS3`
  spec instead.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
