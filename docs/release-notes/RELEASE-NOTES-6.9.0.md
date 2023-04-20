# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.9.0) is a maintenance Iris release.

It:
* Garbage collection has been rewritten, and now operates on all state trees. It is also
  capable of running during chain sync.
  See the documentation, garbage_collection.md, for information on how to configure it.
* API requests that try to retrieve state from heights below the latest GC height will now receive a 410 ("gone")
  return code, instead of a 500 ("internal error"). **Potential incompatibility**: applications that depended on
  the past behavior will need to adapt.
* Provides a way to setup the API webserver to skip reading the http
  request body after an amount of bytes. When the length is too large
  the webserver will close the connection. This is initially meant for
  the Mdw but Nodes with lower RAM spec would benefit too.
  On sys.config, it's an `aehttp`
  config with the property:
    ```
    {protocol_options, [
        {max_skip_body_length, 1000000}
        ]}
    ```
  The minimum recommended value is 400000 to fully read a POST with the max
  amount of bytes/gas that AE protocol allows in a micro block.
  %

With regards of HyperChains it:
* Collects all commitments from the parent chain and feeds them to the smart
  contract. The contract uses those to elect the next leader.
* Derives block difficulty from the total stake supporting this fork on the
  parent chain: only stakers having parent chain commitments supporting the
  previous block hash are considered.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

