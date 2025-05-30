# Hacking the Aeternity Codebase

## Building
See [Build](build.md) for details.
 - Dependencies
   - Ubuntu: `sudo apt install autoconf build-essential cmake erlang
     libsodium-dev libgmp-dev`
   - Mac OS: `brew install erlang@24 openssl libsodium autoconf gmp cmake
     automake`

```shell
git clone https://github.com/aeternity/aeternity.git
cd aeternity
make prod-build
```

The Aeternity build system uses Rebar3 to do the heavy work, and wraps this
in a Makefile for ease of use. To hack on Aeternity you need some basic
knowledge about Rebar3. See [Quick Guide to Rebar](rebar.md) for a
comprehensive introduction.


## Configuration files
 - You can use either `.json` or `.yaml` to specify the user-level
   configuration. By default, the system looks for
   `~/.aeternity/aeternity/aeternity.{json,yaml}` or
   `aeternity.{json,yaml}` in the top directory. You can also set
   environment variables on the form `AE__...`, e.g.
   `AE__HTTP__CORS__MAX_AGE`. See [docs/configuration.md] for details.
 - The system first reads the usual Erlang system configuration files
   (specific per release, in `_build/prod/rel/aeternity/releases/*/`). These
   are generated from the corresponding source files under `config/`:
   - `vm.args` for Erlang VM options.
   - `sys.config` for overriding the Erlang application defaults (the `.app`
     files).


## Running
See [Operation](operation.md) for details.

### Starting the system with an Erlang terminal prompt
```shell
cd _build/prod/rel/aeternity`
bin/aeternity console
```

### Opening an Erlang shell for running a unit or integration test

Rebar lets you open an Erlang shell with one or more profiles applied, such
as `test`. This sets up paths to test apps, etc., which will not be
available in the default profile. By default all apps listed in the release
spec will be started; to avoid this, specify `--apps ""`:
```shell
rebar3 as test shell --apps ""
```
or for system testing
```shell
rebar3 as system_test shell --apps ""
```

The system can then be started manually from the Erlang shell like this:
```erlang
    application:load(aecore),
    TempDir = aec_test_utils:create_temp_key_dir(),
    application:set_env(aecore, keys_dir, TempDir),
    application:set_env(aecore, password, <<"secret">>),
    application:ensure_all_started(aecore).
```
after which you can do your testing. To clean up
the temporary directory that was created, do:
```erlang
 aec_test_utils:remove_temp_key_dir(TempDir).
```


## Aeternity Code structure
- Main applications (in reverse start order), most under the main repo
  (`github.com/aeternity/aeternity.git`) under the `apps` directory; the rest
  will be found under `_build/default/lib`:
  - `aedevmode`
    - (Something about keypairs for testing. Runs `aedevmode_emitter`.)
  - `aesync`
    - The aesync app just launches `aec_connection_sup`, which exists under
      the aecore application. It is a "supervisor for servers dealing with
      inter node communication"
  - `aestratum`
    - An implementation of server side part of the Stratum protocol. The
      purpose of the protocol is to formalize the coordination of information
      exchange between pool server and pool clients. See [docs/stratum.md]
  - `aemon`
    - Network monitoring (disabled by default). Uses statsd backend provided
      by `aec_metrics.erl`. See [docs/monitoring.md]
  - `aehttp`
    - The HTTP API. This app doesn't have any actual child processes of
      its own. It just starts Cowboy endpoints.
    - The Cowboy setup is done in `aehttp_app` which calls
      `aehttp_api_router` to get the endpoint data.
      - The endpoints are specified in `apps/aehttp/priv/oas3.yaml` which
        is used to generate callback modules `oas_endpoints`, `endpoints`
        (the old Swagger version), and `rosetta_endpoints`.
      - `aehttp_api_router` calls these to get the data, and then filters
        it depending on what should be enabled. Note that the important
        `enabled_endpoint_groups` setting is computed in
        `aehttp_app:check_env()`, which runs from a `setup` hook defined
        in `aecore.app.src`.
    - All endpoints enter via `aehttp_api_handler:handle_request_json()`
      which dispatches to one of the modules `aehttp_dispatch_ext`,
      `aehttp_dispatch_int`, or `aehttp_dispatch_rosetta`. These may
      reject a request if the system is overloaded, or put it in a run
      queue for later (see `aec_jobs_queues`). `aehttp_api_handler` also
      does the conversion between JSON-as-text and JSON-as-Erlang-terms
      for request inputs and outputs, using the `jsx` library.
      - For example, the request `GetCurrentKeyBlockHeight` is actually
        handled in the module `aehttp_dispatch_ext`, like this:
        ```erlang
        handle_request_('GetCurrentKeyBlockHeight', Req, Context) ->
            TopBlock = aec_chain:top_block(),
            Height = aec_blocks:height(TopBlock),
            {200, [], #{height => Height}};
        ```
  - `aechannel`
    - State channels (`aesc_...`). The aesc_fsm.erl state machine is
      described by the PlantUML file [docs/state-channels/fsm.puml].
  - `aeapi`
    - A single facade module for the internal API functions. Does not launch
      any child processes, or even any supervisor.
  - `aecore`
    - The Core Aeternity Application supervisor tree. Runs the
      `aec_worker_sup`, `aec_consensus_sup`, and `aec_conductor_sup`. (It
      used to run the `aec_connection_sup` as well, before that was moved
      to the `aesync` app.)
      - `aec_worker_sup`
        - Runs `aec_metrics`, `aec_keys`, and `aec_tx_pool`
      - `aec_consensus_sup`
        - Initially empty
      - `aec_conductor_sup`
        - Runs `aec_conductor` and `aec_block_generator`
          - The (micro)block generator server subscribes to new
            transactions and packs them into microblocks.
          - The conductor is the hub for adding blocks to the local chain.
            It orchestrates the mining and publishing of key blocks and
            signing of microblocks, and handles incoming events about
            synced blocks etc.
          - Important modules:
            - `aec_chain` - API for reading chain information, e.g. reading
            a block or header, finding the genesis block or the top block, etc.
            - `aec_chain_state` - ADT for modifying the chain state. See
            the comments in this module for more details about the chain and
            nodes and forks etc.
            - `aec_db` - Stores nodes and all other persistent data; see [Aeternity Data management](#aeternity-data-management) below
            - `aec_sync` - Synchronizes with other peers
            - `aec_tx_poool` - Pool of unconfirmed transactions
            - `aec_consensus` - Defines the consensus callback behaviour and provides utility functions such as `get_genesis_hash()`.
  - `aecli`
    - The CLI, based on `ecli`. The supervisor is started with no children.
  - `aefate`
    - The FATE virtual machine. A library application, does not start any
      processes.
  - `ecrecover` (`github.com/aeternity/ecrecover.git`)
    - Library for verifying Ethereum signatures.
  - `aega`
    - Library for Generalized Accounts
  - `aeprimop`
    - Library for primitive operations to modify chain state objects.
  - `aeoracle`
    - Library for Oracles.
  - `aens`
    - Naming System library.
  - `aecontract`
    - Library for Contracts
  - `aevm`
    - Aethereum VM clone in Erlang.
  - `aebytecode` (`github.com/aeternity/aebytecode.git`)
    - Library and standalone assembler for Aeternity bytecode, supporting
      both AEVM bytecode and FATE bytecode.
  - `aeserialization` (`github.com/aeternity/aeserialization.git`)
    - Serialization helpers for Aeternity node.
  - `aetx`
    - Library for Transactions ADT
  - `aeutils`
    - Library with various utility functions. Starts a supervisor with no
      children.
  - `aeminer` (`github.com/aeternity/aeminer.git`)
    - Erlang library to work with CPU and CUDA cuckoo miners.
  - `aecuckoo` (`github.com/aeternity/aecuckoo.git`)
    - Cuckoo CPU miner binaries.


## Aeternity Data management
- All persistent state is kept in a Mnesia database, using RocksDB as the storage level backend. The `mnesia_rocksdb` app provides the Mnesia-to-RocksDB integration, and the `erlang-rocksdb` app provides the Erlang-to-C++ RocksDB bindings. On platforms where RocksDB is not supported, like on Windows NTFS volumes, a standard Mnesia database is used.
  - The database is started from two `$setup_hooks` entries in `aecore.app.src`. The first calls `aec_db:check_db()`, which actually launches Mnesia (which is not started by the boot script) and ensures that the schema and tables exist, creating them if needed. The second hook calls `aec_db:start_db()`, which ensures the tables are loaded and ready. (Read about setup hooks in [How the system starts](#How-the-system-starts) below.)
  - The `aec_db:tables/1` function defines the database tables (the schema). The record definitions that specify the fields of the tables are found in `aec_db.hrl`.
  - Apps should generally not call `mnesia` functions directly, but always go via the wrapper functions in `aec_db` or a higher level module like `aec_trees`.

### Important tables
- Tables are accessed via `aec_db` API functions. Normally the direct table operation wrappers like `aec_db:read/2` are not used by application code. Instead, more abstract functions like `aec_db:get_header()` or `aec_db:write_block()` hide how the data is stored in the actual tables. A table name may correspond to a module name, like `aec_blocks`, with functions for manipulating the type of data in the table (but not for accessing the DB). Some things like blocks and headers are not manipulated directly, and are managed by higher level abstraction modules, such as `aec_chain_state` which refers to the blocks as "nodes".
  - `aec_headers` - stores the headers for each block; for a key block, the headers contain all the information - only microblocks have a payload
  - `aec_blocks` - stores the list of transactions that form the payload for each microblock
  - `aec_signed_tx` - stores the actual transactions referred to by the blocks
  - `aec_block_state` - stores additional information about blocks, such as difficulty, fees, forks, fraud, and the root hashes of the "state trees"
  - `aec_chain_state` - stores a mapping from key block height (and hash) to the headers, for cheap lookup, but also stores some additional information such as the genesis hash, the top block, garbage collection info, etc.
  - `aec_tx_location` - maps transaction hashes to the blocks where they occur
  - `aec_tx_pool` - tracks which transactions are in the memory pool
  - `aec_peers` - peer nodes persisted in the DB
  - State Tree tables (see `aec_db:tree_table_name/1`) and their corresponding "tree names". These are simple key/value tables, providing storage for the Merkle trees (see below). The module `aec_trees` defines a record structure that bundles the "handles" for these tables as a single object, making it easy to commit changes to all state tree tables via a single function call. The `aec_block_state` table stores the hashes that point from a block to its state trees.
    - `accounts`      -> `aec_account_state`
    - `calls`         -> `aec_call_state`
    - `channels`      -> `aec_channel_state`
    - `contracts`     -> `aec_contract_state`
    - `ns`            -> `aec_name_service_state`
    - `oracles`       -> `aec_oracle_state`

### Merkle Patricia Trees
- A Merkle tree is a tree where each leaf node is labelled with the hash of a data block, and every inner (non leaf) node is labelled with the hash of the hashes of its child nodes. This lets you verify the integrity of any subtree without needing to look at the rest of the tree.
- A Patricia tree - or rather, "radix tree" - is a kind of prefix tree (or "trie") : the tree does not have to store the keys explicitly, and each entry is stored under the shortest key prefix path. E.g. if an entry has a key which in binary is 1011..., it is stored under the path right-left-right-right-... (with additional optimizations for keeping the tree as small as possible). Prefix trees are specialized for bitstring-like keys - in Aeternity the keys are hashes.
- The `aeu_mtrees` implementation in Aeternity is used for the "state trees". They are parameterized so they can store the actual nodes either in a database table or in an in-memory data structure like a map or gb-tree. See `aec_trees:new()` to see how this is done. The module `aeu_mp_trees_db` is an abstract backend to be instantiated with a specific database and cache implementation. The module `aec_db_backends` defines which concrete backend and table is used for each of the tree tables above, creating backend instances which then get used by the `aeu_mtrees`.
- An important feature of mtrees and its backends is that store operations do not perform any side effects right away - they just insert the changes in a write cache. Once ready, the cached changes can all get committed to the database via a single call; see `aec_trees:commit_to_db()`.
- The function `aec_chain:get_block_state()` reads the block state table entry to get the root hashes for the state trees of the block, then instantiates an `aec_trees` record with the corresponding root hash and correct backend for each state tree, via `aec_trees:deserialize_from_db()`.


## How the system starts
- There is no single function call to start the Aeternity system. There
  is a start script (`_build/prod/rel/aeternity/bin/aeternity`) which
  is generated by Rebar3 when you run e.g. `make prod-build` or `make
  prod-package`. The package would typically be installed under
  `~/aeternity/node` and it is assumed that you start the system from
  the install directory (or directly from the build directory). You
  typically run it as `bin/aeternity daemon` or `bin/aeternity
  console`.
  - The start script is a modified version of the "extended start
    script" that Rebar3 would normally generate from its standard
    template. The source file for the Aeternity version is located in
    `scripts/aeternity_bin`. This file should be kept up to date with
    changes in the upstream Rebar3 version (which is part of `relx`).
- The start script starts Erlang with a custom boot file generated by
  Rebar3, named `start.boot` or `aeternity.boot` (in
  `_build/prod/rel/aeternity/releases/*/`). It is specific for each release
  and specifies exactly what processes and applications should be started
  when the Erlang system boots. (The `.boot` file is in a binary format
  that the system can read at boot time without needing to load any other
  modules such as for parsing text. To see what the boot file does, look at
  the corresponding source file `start.script` (or `aeternity.script`)
  instead.)
  - That the system starts from a boot file means that applications not
    listed in the boot script will not be available in the code path when
    the system is running, even if they are normally in the standard
    Erlang/OTP distribution; e.g., `debugger`, `wx`, or `parsetools`. If
    wanted, such extras must be added manually, or run from a separate
    Erlang instance.
  - Multiple releases can be installed alongside each other, and the code
    paths in the boot script typically name the versions of the
    applications (in the `lib` directory under the installation root), so
    e.g. release 1.1 could be using the version `"lib/xyz-2.0.3"` of
    application `xyz`, while release 1.2 uses the version
    `"lib/xyz-2.1.1"`. The start script (`bin/aeternity`) picks the
    release version to use.
- The `.boot` and `.script` files are generated automatically by the
  release-building tools of Rebar3, using the `relx` specification section
  of the `rebar.config` file. This is where you list all the Erlang
  applications that should be included in the release and started when the
   Aeternity system boots. (They will be "started" regardless of whether
  they actually start any processes. This loads the app configuration.) The
  start order in the `.boot` file is made to obey the application
  dependencies found in the individual `*.app` files (usually generated
  from `*.app.src` files) that provide the per-application metadata: for
  example, the `apps/aehttp/src/aehttp.app.src` file specifies that
  `aecore` must be started before `aehttp` can start. Hence, when the
  Erlang system boots, it will launch all specified applications in a
  suitable order, and when all are running, the system is up. There is no
  specific single entry point to the system.
  - The boot script also includes the app configuration from the `{env,
    ...}` sections of the `.app` (or `.app.src`) files *at build time*, and
    sets these configurations as the system boots. *Modifying the `.app`
    files in the installed system has no effect.* Use the `sys.config` or
    command line options to override the build-time configuration.
  - Furthermore, *Rebar3 doesn't rebuild dependency apps (under
    `_build/default/lib`) if they get modified*, so updating e.g.
    `_build/default/lib/lager/src/lager.app.src` will have no effect on
    `lager.app` (and hence not on the produced release build) - you must
    delete the existing `_build/default/lib/lager/ebin/lager.app` file to
    force Rebar3 to rebuild it.
- The (nonstandard) `setup` application, which is assumed to start very
  early in the boot sequence, provides extra startup configuration magic:
  - It scans all application configurations for entries with the key
    `'$setup_hooks'`, specifying callback functions to be executed by
    `setup`. (See e.g. `aeutils.app.src`.)
    - Because the boot script loads all application configurations and
      modules before it starts the first application, the full list of
      applications and their configuration is known when `setup` is
      started.
    - The Aeternity system uses these callbacks to read the
      `aeternity.{yaml,json}` (`aeu_env:read_config()`) file, inject
      overrides from OS environment variables `AE__...`
      (`aeu_env:apply_os_env()`), and load plugins
      (`aeu_plugins:load_plugins()`) before the rest of the system starts,
      as well as perform sanity checks on configurations
      (`aecore_env:check_env()`, etc.), and most importantly, start the
      database (`aec_db:start_db()`).
  - It has "smart" `get_env()` functions which can perform advanced
    variable expansion on the configuration values. E.g., if an application
    `x` has a configuration entry `{log_dir, "$HOME/log"}`, then calling
    `setup:get_env(x, log_dir)` will return something like
    "/home/username/log". *This only works on variables defined by `setup`
    itself, not operating system environment variables. Note in particular
    that `$HOME` does not mean the current user's home directory - it
    refers to the `setup` configuration key `{home, Dir}` meaning the
    root directory of the Aeternity system; if not set, the current
    directory is used.*
  - Setup has a configuration option `data_dir` which the Aeternity system
    uses to know where its database is located. The directory needs to
    already exist and be populated at system start, else the startup fails.
- The (nonstandard) `app_ctrl` application provides additional control over
  the start order in the system. Normally, the applications are started in
  the order listed in the `relx` specification of `rebar.config`, modified
  to obey the dependencies listed in the individual `.app.src` files. This
  means that applications can specify that they must be started *after*
  other applications that they know about and depend on, but application
  `x` cannot specify that it needs to start *before* another application
  `y` which is unaware of `x` and whose dependencies (in its `.app` file)
  cannot be modified.
  - The `app_ctrl` app hooks into the kernel application, which is always
    the first to start, by configuring `app_ctrl_bootstrap` to run as
    (dummy) handler of the logger functionality in the kernel. This is done
    in the `sys.config`. When the kernel app starts, this launches the
    `app_ctrl_server` process (but not the `app_ctrl` application itself).
  - The `app_ctrl_server` looks for configuration both in the normal
    `app_ctrl` app environment, and by scanning other applications for
    entries with the key `'$app_ctrl'` (using functionality from `setup`;
    see above). In Aeternity, this can be found in the `aecore.app.src`
    file.
  - The `app_ctrl` configuration can specify per application that the app
    needs to be started before certain other apps. It can also define
    "roles", which are sets of apps, and "modes", which are sets of roles.
    Applications that are not explicitly mentioned in the configuration are
    left to the standard application controller.
  - If you try to make an application in Aeternity depend on (start after)
    one of those applications that are managed by `app_ctrl`, such as
    `aehttp`, then you will get a crash during startup with error messages
    containing `{orphans,[...]}` and `apps_not_found: [...]`. To fix this
    you must also add your app to the same "roles" in the `'$app_ctrl'`
    section of `aecore.app.src`.
  - When the real `app_ctrl` application is finally started, it just sets
    up a supervisor and a worker process which acts as a proxy that links
    itself to the already running `app_ctrl_server` process, so that the
    application crashes if the server process crashes.
- Logging is done via the Lager app. A handler `aeu_lager_logger_handler`
  for the standard OTP logger is also set up in the `sys.config`, which
  forwards standard log messages to Lager.
  - The `aeutils.app.src` file configures a hook for the `setup` app,
    making it call `aeu_logging_env:adjust_log_levels()` when `setup`
    starts. (Note that `aeutils` configuration must thus be loaded before
    `setup` runs, which it will be when running from a boot script.) This
    will also call `aeu_logging_env:expand_lager_log_root()` to ensure
    that `lager` has its `log_root` configuration set, using
    `setup:log_dir()` as the default. Furthermore it rewrites the log
    root setting to be an absolute path, to ensure that the logging is
    not affected by changes to the current working directory of the
    Erlang VM during execution.
  - As soon as lager starts, it will create the log directory and all log
    files using its current configuration.
  - Since `setup` and `lager` don't know about each other's existence,
    their `.app` files do not specify any dependency between them. Their
    relative order in the `relx` specification thus decides their actual
    order in the boot script.
  - The `lager` configuration in `sys.config` sets up both a handler that
    writes to the console, and a handler that writes to the
    `aeternity.log` logfile. It also configures additional logging sinks,
    for which corresponding modules are generated dynamically, so that
    the sink whose name is `epoch_mining_lager_event` can be used by
    calling `epoch_mining:info(...)`, and so on. Hence you will not find
    a source module named `epoch_mining.erl` in the codebase. Most of
    these extra sinks will not log to the console, only to log files.
