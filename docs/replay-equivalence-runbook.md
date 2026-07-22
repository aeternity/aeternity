# Batch-MPT deep replay-equivalence runbook

This is the procedure a node operator runs on their own infrastructure, against their own synced
mainnet chain-DB snapshot, to prove the batched binary (this worktree, pinned at `c05f45ea` or
later) recomputes byte-identical state roots for a real, high-activity slice of mainnet history.
It drives the harness module `apps/aecore/test/aec_replay_harness.erl`.

**This never touches a live/production node.** Every step below operates on a disposable COPY
of chain data, offline, with no P2P/mining/HTTP surface exposed to the network. See §5 Safety.

## 1. What "pass" means

For every block in the chosen height range, recomputing its state via the batched binary's real
`aec_chain_state:repair_block_state/1` path must reproduce **exactly** the `state_hash` the block's
header already records (written by whatever binary originally synced that height — almost
certainly the current, unbatched `releases/stable`/`master` build). One mismatch anywhere in the
range is a **hard fail** — loud, not silent, and not waivable (see §4).

## 2. What you need

- **A synced mainnet chain-DB snapshot**, containing at minimum:
  - The Mnesia/RocksDB block-index and header tables (`aec_headers`, `aec_blocks` or equivalent —
    whatever `chain.db_backend`/`chain.db_path` produced) covering genesis through your chosen
    `ToHeight`.
  - The **block-state table** (`aec_block_state` / whatever holds the persisted MPT roots per
    block hash) covering at least up to your chosen `FromHeight` (the checkpoint — see §3.2). It
    does **not** need to be present for heights above `FromHeight`; the whole point of this
    runbook is to **recompute** those with the batched binary and compare.
  - This repo already has an established bootstrap path for exactly this: the root
    `Makefile`'s `make snapshot` / `make restore` targets fetch and md5-verify a published
    mainnet DB archive (`SNAPSHOT_URL`, `aeternity-database-backups` S3 bucket) and unpack it into
    `DATA_DIR`. Use that, or an equivalent export of a synced node's own `data/` directory —
    either way, treat it as **read material
    only**: never restore into or run this against the DATA_DIR of a live/operational node.
- **Height range.** Pick a `FromHeight`/`ToHeight` inside your snapshot's covered range that is
  rich in the activity this diff actually touches: contract create/call (store writes), oracle
  register/query/response, AENS preclaim/claim/update/transfer/revoke and natural TTL expiry, and
  ordinary spends with hot (repeatedly-touched) accounts. Any recent mainnet range easily
  qualifies — contracts, AENS and oracles have all been part of mainnet for a long time, so there
  is no need to hunt for "where activity starts."
  Default recommendation: **the most recent ~50,000 blocks your snapshot has** (richest, most
  representative mix); narrow to a specific window if you're reproducing a known incident instead.
  `ToHeight - FromHeight` of a few thousand key-block heights is enough for a meaningful proof;
  start smaller (e.g. 500) for a first dry run before committing to a long unattended range.
- **Protocol-activation boundaries — REQUIRED.** A heavy-activity range is necessary but **not
  sufficient**: the replay MUST also cross **at least one real historical hard-fork /
  protocol-activation boundary** end-to-end. That is where batched **hard-fork contract
  migration** (`aec_block_fork` deployment under per-microblock batching) actually runs on real
  state. On `ae_mainnet` the boundaries are:

  | Protocol | Effective height |
  |---|---|
  | Minerva (v2) | 47,800 |
  | Fortuna (v3) | 90,800 |
  | Lima (v4) | 161,150 |
  | Iris (v5) | 441,444 |
  | Ceres (v6) | 941,700 |

  Choose a `FromHeight`/`ToHeight` that **straddles** one (e.g. `FromHeight=941600`, `ToHeight=941800`
  to cross Ceres). A single clean run crossing every reachable boundary in the snapshot is the
  strongest evidence.
- **The batched binary**, built from this exact worktree (`aeternity-batch-mpt`, currently pinned
  at commit `c05f45ea`) — not from `releases/stable`/`master`.
- **The OTP-26 (or OTP-24) CI builder image**, `aeternity/builder:focal-otp26` — do not attempt a
  native build on a host with an older OTP; see the `_build`-profile note in §3.1.
- **Disk headroom** for a full copy of the snapshot (never operate in place on the original).

## 3. Procedure

### 3.1 Build the batched release

From the `aeternity-batch-mpt` worktree root, in the pinned builder image (`docker run --rm
--user 0:0 -v $PWD:/home/builder/aeternity -w /home/builder/aeternity
aeternity/builder:focal-otp26 bash -c '<cmd>'`):

```
./rebar3 as prod release
```

This produces `_build/prod/rel/aeternity/bin/aeternity` — the real production release, built from
the batched code. (`--user 0:0` matters: if the `_build` tree was previously populated by a
container using a different mount path, you may see dangling `_build/test/lib/*` symlinks; if a
`fate_eval` hook then fails with `undefined function ...:get_ops/0`, regenerate the symlink to
point at the corresponding `_build/default/lib/<app>` directory under this container's mount.)

### 3.2 Prepare an isolated snapshot copy

```
cp -a /path/to/synced-mainnet-snapshot /path/to/scratch/replay-datadir
```

Never point any step below at the original. `FromHeight` is whatever height your snapshot's
`aec_block_state` (or backend equivalent) already has computed and trusted — the checkpoint the
replay starts from unmodified. `aec_chain_state:repair_block_state/1` (the primitive
`aec_replay_harness:replay_repair_range/2` drives) **mutates the stored trees** for every height
it touches, cascading each recompute onto the batched-code result of the height before it — that
is intentional (it is exactly how a real re-derivation from a checkpoint has to work) but it is
also why this must never run against anything you still need un-mutated afterward.

### 3.3 Start the batched binary, offline, against the copy

Configure (env-var override or `aeternity.yaml`, on top of the release's default config):

- `chain.db_path` → the scratch copy's data directory.
- `chain.db_backend` → whatever the snapshot actually is (`rocksdb` / `mnesia` — matches what
  `chain.persist=true` was set to when it was synced).
- `fork_management.network_id` (`AE__FORK_MANAGEMENT__NETWORK_ID`) → `ae_mainnet`.
- Leave `mining.autostart` at its default (`false`) — do not enable mining.

Start it **with no network egress at all** — run the container with `--network none` (or
equivalent host-level firewalling if not using Docker), and do **not** publish the sync port
(3015) or the HTTP API port (3013) to anything reachable. This node must never dial a peer, never
accept a peer, never serve a public API — it only needs to boot far enough to have `aec_db`,
`mnesia`/`rocksdb`, and the `aecore` application's non-networked pieces up so an attached shell can
call chain-state functions directly. If your release's boot sequence insists on starting
`aehttp`/`aesync` regardless of config, that's fine as long as `--network none` holds — the
guarantee is "no live node ever connects to the mainnet P2P network," not "no listener exists."

```
docker run --rm -it --network none \
  -e AE__FORK_MANAGEMENT__NETWORK_ID=ae_mainnet \
  -e AE__CHAIN__DB_PATH=/data \
  -v /path/to/scratch/replay-datadir:/data \
  -v $PWD/_build/prod:/home/aeternity/node \
  <image-or-just-run-the-release-binary-directly> console
```

(Adjust to however the release is normally run — the top-level `Makefile`'s `make start` target
covers the container plumbing; the only differences here are `--network none`, the scratch
`DATA_DIR`, and running the **batched** release rather than the published image.)

### 3.4 Drive the harness from the attached console

Once you have an Erlang shell attached to the running batched release (`console` gives you one
directly; otherwise `bin/aeternity remote_console`), the harness module ships compiled into the
`test` profile only (`apps/aecore/test/aec_replay_harness.erl`) — a `prod` release will not have it
on its code path by default. Load it directly from source in the console:

```erlang
{ok, _} = compile:file("apps/aecore/test/aec_replay_harness.erl", [{outdir, "/tmp"}]),
code:add_patha("/tmp"),
{module, aec_replay_harness} = code:ensure_loaded(aec_replay_harness).
```

Then run the range:

```erlang
FromHeight = 1300000,   %% your checkpoint — already-trusted in the snapshot
ToHeight   = 1300500,   %% first dry run: keep this small
Report = aec_replay_harness:replay_repair_range(FromHeight, ToHeight),
aec_replay_harness:print_report(Report).
```

### 3.5 Read the result

- **`REPLAY PASS: N/N blocks byte-identical ...`** on stdout, `Report = {ok, Entries}` — every
  block in `(FromHeight, ToHeight]` recomputed to the exact recorded `state_hash`. Capture the
  full console transcript (heights, hashes, timings) as the raw-run evidence.
- **`REPLAY FAIL after N good blocks — DIVERGENCE: ...`** on stderr, `Report = {error, BadEntry,
  GoodEntries}` — a real, reachable root-hash divergence between the batched code and the
  original recorded chain. **Do not treat this as a data problem and retry past it.** Capture the
  divergent height, block hash, and both root hashes (`got` vs `expected`) verbatim, and stop —
  see §4.

## 4. If it fails

A divergence found here is CRITICAL and consensus-adjacent by construction (it means a batched
node would compute a different state root than the rest of the network for a real, already-mined
block). Do not work around it locally and do not retry past it: there is no pass-with-risk for
this class of defect. Capture the divergent height, block hash, and both root hashes (`got` vs
`expected`) verbatim, record them in the product decision log, and escalate for root-cause
analysis before the change goes any further.

## 5. Safety

- **Never run any step of this runbook against a live, operational, or production node's data
  directory.** Always a disposable copy (§3.2).
- **No network egress during the replay** (`--network none` / firewalled) — this binary must never
  dial or accept a P2P connection, mine, or serve a public API while doing this recompute.
  Historical mainnet data is used read-only as fixture data; the node must never mine or validate
  against the live network as part of this exercise.
- **No mainnet keys or funds are involved anywhere in this procedure.**
- **`repair_block_state/1` mutates the DB it runs against** (§3.2) — this is expected and is why
  the copy is disposable, not the source snapshot.
- This is a **read/recompute-only exercise**: nothing produced here is ever broadcast, gossiped,
  or submitted to any network.
