# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.6.0) is a maintenance Lima release.

It:
* Enchances the p2p noise protocol with an optional setting to ask a peer
  for the version of their node. A node operator can set one's node to not
  respond to this message if one considers this a private information. This is
  to be used for monitoring the network's health.

* Enhances FSM behaviour: when the initiator is offline, allows the responder
  to stay online waiting for it even if the timeout timer is reached.

* Configuration values can now be set using OS environment variables, where the environment variable name is on the form `AE__k1__k2`, e.g. `AE__CHAIN__PERSIST=true`. Note that two underscores are used to separate each level. Structured values must be JSON-encoded. The prefix `AE` can not be customized. The environment variables are applied after reading an available config file, and all values are checked against the schema. See `docs/configuration.md`.

* Introduces persistence of the peer pool: if the node flag for disc
  persistence is set, after a node restart old peers are loaded from the DB.
  Trusted peers are provided to the node from the config file so they are already
  persisted. Since the time being off is unknown, all peers are loaded as
  `unverified`, even if they had been `verified` before the node stop.

* Changes the default value and significantly speeds up the TCP probes. Those
  used to be done roughly once every 2 minutes while now they are executed
  every second. Note that this is done only if there are peers to be checked:
  if a peer is missing, there is a grace period in which we mark it as
  'suspended', waiting for its return. If all peers are checked, we wait
  longer - until either a new peer is added or there is a not-suspended peer.
  Basically the probes come on demand. Even despite the probes are fast, there
  is a maximum size of currently ongoing probes.

* Fixes the process of probing verified peers. Until now we were probing only
  unverified peers. If a peer responds to the probe - we consider it verified.
  We downgrade a verified peer only after we fail connecting to it. This
  resulted in amounting peers that used to be verified at one point of time.
  This is fixed now as we probe verified peers as well. What is more is, that
  this PR plays really well with PR #3365.  This means that if a peer goes
  down, we will find it significantly sooner and will downgrade it as an
  unverified one. This is important as a nodes shares to other nodes only
  peers one considers to be verified.

* Added support for reporting chain events during contract evaluation. These events
  are published internally as 'tx_events' (available e.g. for plugins), and can also
  be fetched via the HTTP 'dry-run' method. The events are presented as 'dummy'
  transactions, unsigned, reflecting the details of the respective events.

* Fixes a bug: there are special trusted peers that are never deleted nor
  downgraded. Those are quite unique as the node consideres them to be
  available. One can pick their own set of trusted peers but this was not
  really effective: the default peers were always included as a bonus.
  This PR fixes it: if the new config flag is set to `false` - the defaults
  are omited. The flag is called `include_default_peers` and by default it
  is true.

* There used to be multiple copies of the same peers in peers pool. This
  resulted in propagation of same peers with different Peer IDs through the
  network. Now duplicates are cleaned up.

* Some further peers fine tuning: we used to treat peers that refuse
  connecting to us as they were malicious. This was the case no matter if
  peers were active and part of the network and only simply refusing to open a
  `enoise` connection to our node (most likely because their inbound pool is
  staturated already). This is now changed and if the node is responding but
  refusing to open an `enoise` connection - we consider it still a valid and
  verified participant of the network.

* Improves sync's peer propagation: peers are split into verified and
  unverified. We had allowed a grace period for peers to be down while the
  node still considers them as verified and broadcasts them as such. Once a
  verified peer is being detected as down, it enters a standby state that
  allows them to stay verified.
  This PR tightens the process - once a peer is being detected to be
  unreachable - it is being downgraded from verified to unverified
  immediately. The processing of unverified being deleted is left unchanged -
  they still enter the standby mode for a few times before being removed.

* Fixes inconsistency of the `/status` endpoint

* Provides some fine tuning to the supervision tree of the conductor. This
  will prevent the node making too many recovery attempts after a crash.

* Enhances FSM behaviour: when the initiator is offline, allows the responder
  to stay online waiting for it even if the timeout timer is reached.

* Adds an explicit settings for `local_lima_testnet`. So far those were
  implicit as Lima is the currently running release. The MDW though uses a
  macro to parse those and if there is no explicit function head - it fails.
  This aims at providing the MDW with capabilities to work with a local Lima
  test environment.

* Due to MacOS High Sierra reaching EOL status we no longer build and provide release binaries for MacOS High Sierra. MacOS Mojave and above is supported.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

