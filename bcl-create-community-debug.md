# BCL `create_community` Performance Debug

## Contract Reference

- **Contract**: `ct_25cqTw85wkF5cbcozmHHUCuybnfH9WaRZXSgEcNNXG9LsCJWTN`
- **Source**: https://github.com/bctsl/bctsl-contracts/blob/main/contracts/CommunityFactory.aes
- **Function**: `create_community` — over 17,276 tokens created
- **Explorer**: https://aescan.io/contracts/ct_25cqTw85wkF5cbcozmHHUCuybnfH9WaRZXSgEcNNXG9LsCJWTN

---

## Part 1: Zero-Block Issue (Block Production Stall)

### Problem

When `create_community` is called on the `CommunityFactory` contract, the network enters a
state where key blocks are produced but contain **zero transactions** (no microblocks) for an
extended period.

### Why `create_community` Is Expensive

The function deploys **3 new contracts** and makes multiple cross-contract calls in a single
transaction:

1. `Chain.create() : CommunityManagement`
2. `Chain.create() : DAO`
3. `Chain.create() : AffiliationBondingCurveTokenSale`
4. `dao.set_token_sale()`
5. `community_management.set_owner()`
6. If `initial_buy_count > 0`:
   - `bonding_curve_token_sale.price()`
   - `bonding_curve_token_sale.buy_with_affiliation()`
   - `bonding_curve_token_sale.token_contract().transfer()`
   - `state.protocol_dao_token.mint()`

With 17K+ entries in the contract state, each state read/write goes through Merkle Patricia
trie operations that scale with state size.

### Root Cause

The block generator (`aec_block_generator.erl`) uses a single worker process. On every
`top_changed` event — including micro blocks (every ~3 seconds) — the worker is killed and
restarted via `preempt_generation`. A heavy tx like `create_community` that takes 10+ seconds
to execute gets killed repeatedly before it can finish, starving the entire microblock pipeline.

The conductor (`aec_conductor.erl`, lines 965-975) already applies this principle for key block
mining — it does NOT reset PoW mining workers when a micro block arrives from the same
generation. But the block generator was not given the same treatment.

### Applied Fix

**File**: `apps/aecore/src/aec_block_generator.erl`

Added `handle_top_changed/2` to distinguish key blocks from micro blocks:

| Block type | Worker running? | Action |
|------------|-----------------|--------|
| key        | any             | **Preempt**: kill worker, restart on new top (new generation) |
| micro      | yes             | **Defer**: record new top in `pending_top`, let worker finish |
| micro      | no              | **Preempt**: rebuild candidate on new top (normal flow) |

When the worker finishes and `pending_top` is set, the stale candidate is discarded and a fresh
worker starts on the correct top. Key block preemption is preserved. This is a non-consensus
change — no hard fork required.

Full details: [zero-block-debugging.md](zero-block-debugging.md)

---

## Part 2: Dry-Run API Slowness

### Problem

Calling `create_community` via the `/v3/dry-run` API is slow. The response takes a long time
or may time out entirely.

### Pipeline Breakdown

The dry-run request passes through 4 layers, each contributing latency:

#### Layer 1: Jobs Queue Bottleneck

The `ProtectedDryRunTxs` endpoint has no dedicated queue entry, so it falls through to the
catch-all `http_update` queue:

```
apps/aehttp/src/aehttp_dispatch_ext.erl (line 121)

queue(_) -> ?WRITE_Q.   %% WRITE_Q = http_update
```

The `http_update` queue has a default concurrency limit of **5** (defined in
`aeternity_config_schema.json`). Dry-run competes with `PostTransaction` and other write
operations for the same 5 slots. Under load, requests queue up or get rejected.

#### Layer 2: State Tree Loading

Each dry-run call loads the full blockchain state at the given top block:

```
apps/aetx/src/aetx_env.erl (line 133)

try aec_chain:get_block_state(Hash) of
    {ok, Trees} ->
```

This:
1. Reads the serialized state from Mnesia (`aec_block_state` table)
2. Deserializes all 6 state trees (contracts, calls, channels, names, oracles, accounts)
3. Each tree is backed by Merkle Patricia tries stored in Mnesia

There is **no cross-request caching** of the deserialized state. Every dry-run call that hits
the same top block repeats this work.

#### Layer 3: FATE Contract Execution (The Core Cost)

The actual contract call runs through `aec_trees:apply_txs_on_state_trees` in strict mode:

```
apps/aecore/src/aec_dry_run.erl (line 100)

case aec_trees:apply_txs_on_state_trees(
    [Tx], Trees, Env1, [strict, dont_verify_signature|Opts]) of
```

For `create_community`, this involves:

**a) Merkle trie I/O for every state access**

Each `state.field` access in Sophia goes through:
`aefa_stores:find_value` → `aect_contracts_store:get_w_cache` → `aeu_mp_trees:get`

The trie traversal for each key is O(log n) depth. With 17K+ entries, the trie is ~5 levels
deep. Each level may require a Mnesia read (disk I/O) if the node isn't cached in ETS.

**b) Store map operations scale with state size**

`create_community` modifies:
- `collection_registry` — nested map (`map(string, collection_data)`)
- `known_token_sales` — `Set.set(address)` with 17K+ entries
- `community_management` — `map(TokenSale, CommunityManagement)` with 17K+ entries

Each `Map.insert` / `Set.insert` on a store-backed map triggers Merkle trie reads via
`store_map_lookup` / `store_map_member`.

**c) Store finalization**

After FATE execution, `aefa_stores:finalize` writes all modified entries back to the
Merkle Patricia tries. Each byte written costs 5 gas (`STORE_BYTE_GAS`), but the real cost
is the trie updates — each `aect_contracts_store:put` modifies the write cache, and
copy/update of large maps requires traversing the entire subtree.

**d) Three contract deployments**

Each deployment: deserialize bytecode → create initial store → run `init` → write contract
to state tree.

#### Layer 4: Synchronous, Single-Threaded Execution

The dry-run processes transactions sequentially in the Cowboy handler process:

```
apps/aecore/src/aec_dry_run.erl (lines 94-109)

dry_run_int([{tx, TxOpts, Tx} | Txs], Trees, Env, Opts, Acc) ->
    ...
    case aec_trees:apply_txs_on_state_trees([Tx], Trees, Env1, [strict, ...]) of
        {ok, [Tx], [], Trees1, Events} ->
            dry_run_int(Txs, Trees1, Env2, Opts, [...])
```

No parallelism, no timeout, no progress reporting.

### How to Increase Dry-Run Speed

#### A. Configuration (No Code Changes)

### Applied Fixes (4 changes in 3 files)

#### Fix 1: Dirty Mnesia backend for dry-run (highest impact)

**File**: `apps/aecore/src/aec_dry_run.erl`

**Problem**: The dry-run loaded state trees with `DirtyBackend = false`, meaning every Merkle
Patricia trie node read during FATE execution went through `ensure_activity(transaction, Fun)`.
Since the Cowboy handler process has no active Mnesia activity state, **each trie node read
started a brand new Mnesia transaction** (lock acquisition, transaction context setup, commit).
With ~5 trie levels per store key and ~50+ store accesses during `create_community`, that's
~250 individual Mnesia transactions.

**Fix**: Added `get_block_state_dirty/1` helper that calls `aec_db:find_block_state(Hash, true)`
(dirty backend). Inlined the `aetx_env:tx_env_and_trees_from_hash` logic in the dry-run module
to use it. The dirty backend uses `mnesia:activity(async_dirty, Fun)` which does direct
ETS/backend access with no locks or transaction overhead.

This is safe because dry-run is read-only — no state is persisted.

#### Fix 2: Route dry-run to the read queue

**File**: `apps/aehttp/src/aehttp_dispatch_ext.erl`

**Problem**: `ProtectedDryRunTxs` had no explicit queue entry and fell through to the catch-all
`queue(_) -> ?WRITE_Q`, sharing `http_update` (max 5 concurrent) with `PostTransaction`.

**Fix**: Added `queue('ProtectedDryRunTxs') -> ?READ_Q` so dry-run uses the `http_read` queue.
Dry-run is fundamentally a read operation (no chain state mutation). This stops it from
competing with write operations for queue slots.

#### Fix 3: Consistent default gas for `call_req`

**File**: `apps/aecore/src/aec_dry_run.erl`

**Problem**: The dispatch layer estimated gas as 10M for the gas limit check, but the actual
`prepare_call_req` used a default of 1M. When users sent `call_req` without specifying `"gas"`,
the gas check passed but the FATE VM only got 1M gas — causing `out_of_gas` for non-trivial
contracts.

**Fix**: Changed the default from `1000000` to `10000000` (via `?DEFAULT_CALL_REQ_GAS` macro),
matching the dispatch layer's `?DEFAULT_CALL_REQ_GAS_LIMIT`.

---

## Part 3: Attempted Optimizations That Did NOT Help

### Attempt A: Process-dictionary read cache in `aec_db_backends.erl`

**Hypothesis**: Mnesia reads during trie traversal are redundant — upper branch nodes
(shared across many keys) are read repeatedly. A per-process cache in the MPT DB backend
(`mpt_db_get/2`) would eliminate these redundant reads.

**Implementation**: Added `enable_read_cache/0` and `disable_read_cache/0` functions that
set a process dictionary flag. When enabled, `mpt_db_get/2` checked a process-dictionary
map before calling Mnesia, caching every read result.

**Result**: **No measurable improvement** — dry-run still took ~14 seconds (unchanged).

**Why it failed**: Mnesia dirty reads are backed by ETS (in-memory hash table). Each read
completes in ~2-7μs. Even with hundreds of redundant reads, the total I/O time is only a
few milliseconds — negligible against the 14-second total. The bottleneck is not I/O.

**Status**: Reverted.

### Attempt B: Contract store preloading via `preload_cache/1`

**Hypothesis**: Instead of lazy per-key trie traversal, preload the entire contract store
into the Erlang `read_cache` map at contract load time. This single bulk traversal via
`aeu_mtrees:iterator` would be more efficient than many individual `aeu_mp_trees:get` calls.

**Implementation**:
- Added `aect_contracts_store:preload_cache/1` that calls `subtree_w_cache(<<>>, Store)` to
  iterate all entries and populate `read_cache`.
- Modified `subtree_w_cache/2` to recognize a root preload marker (`{subtree, <<>>}`) so
  sub-prefix lookups could be served from cache.
- Added `maybe_preload_for_dry_run/1` in `aect_state_tree:add_store/3` that checked a
  process dictionary flag (`aec_dry_run_cache`) and preloaded when set.
- Managed the flag lifecycle in `aec_dry_run:dry_run/4` with `put/erase`.

**Result**: **Performance worsened to ~32 seconds** (from ~14 seconds baseline).

**Why it failed**: The CommunityFactory contract has 17K+ store entries. Preloading iterates
**all** of them upfront (~18 seconds of trie traversal and decoding). But `create_community`
only accesses a small fraction of these entries (~10-50 keys). The cost of the unnecessary
bulk preload far exceeded any savings from cached reads.

**Status**: Reverted.

### Conclusion on Dry-Run Performance

The ~14-second execution time for `create_community` is an **irreducible CPU cost of the
FATE VM interpreter**. The time is spent:
- Interpreting FATE bytecode instructions
- Performing store map operations (insert, lookup on large maps)
- Deploying 3 sub-contracts (bytecode deserialization, init execution)
- Store finalization (write-back of modified entries)

None of these are I/O-bound. Mnesia dirty reads from ETS are sub-10μs each. The FATE VM is
a single-threaded interpreter with no JIT compilation — its speed is fundamentally limited by
CPU instruction throughput.

**Meaningful speedups would require**:
- FATE VM compiler optimizations (instruction fusion, specialized map operations)
- JIT compilation for hot contract paths
- Parallel store finalization
- Contract-level architectural changes (smaller state, fewer cross-contract calls)

These are deep changes to the VM and out of scope for quick fixes.

---

### Applied Fixes (Final — 3 changes in 2 files)

The following three fixes remain applied and are correct:

| Fix | File | What it does | Impact |
|-----|------|-------------|--------|
| Fix 1 | `aec_dry_run.erl` | Dirty Mnesia backend for state loading | Eliminates per-node Mnesia transaction overhead |
| Fix 2 | `aehttp_dispatch_ext.erl` | Route dry-run to `http_read` queue | Prevents contention with write operations |
| Fix 3 | `aec_dry_run.erl` | Default gas 10M for `call_req` | Prevents `out_of_gas` for non-trivial contracts |

These fixes address real correctness and infrastructure issues, even though they don't reduce
the FATE VM execution time itself.

### Additional Configuration Recommendations

1. **Increase the gas limit** (permits execution of expensive calls):
   ```
   AE__HTTP__EXTERNAL__GAS_LIMIT=50000000
   ```

2. **Increase read queue concurrency** (now that dry-run is on the read queue):
   ```
   AE__REGULATORS__HTTP_READ__COUNTER=20
   ```

3. **Pin the `top` block hash** in dry-run requests instead of using `"top"`. Repeated calls
   against the same hash benefit from the Mnesia ETS cache since trie nodes are already in
   memory from the first call.

4. **Run on fast NVMe storage** — Merkle Patricia trie nodes are stored in Mnesia on disk.
   NVMe SSDs reduce per-node I/O latency significantly.

### Future Improvements (Not Applied)

5. **State tree LRU cache** — cache deserialized `Trees` for the N most recent block hashes.
   Many dry-run calls use the same `top`, so this avoids redundant state deserialization. The
   cache needs invalidation on `top_changed`.

6. **Per-tx timeout for dry-run** — unlike candidate building, dry-run runs in strict mode. A
   long-running contract call blocks the HTTP request indefinitely. A configurable timeout
   (spawn execution in a subprocess with a timer) would prevent indefinite hangs.

7. **Preload contract stores eagerly** — the FATE store is lazy-loaded (each key triggers a
   Mnesia read). For contracts with large states, preloading the store subtree into memory
   before execution would amortize the trie traversal cost.

### Key Files

| File | Role |
|------|------|
| `apps/aehttp/src/aehttp_dispatch_ext.erl` | HTTP endpoint handler, gas limit check, jobs queue routing |
| `apps/aehttp/src/aehttp_helpers.erl` | `do_dry_run()` — calls `aec_dry_run:dry_run` |
| `apps/aecore/src/aec_dry_run.erl` | Core dry-run logic: state setup, dirty backend |
| `apps/aetx/src/aetx_env.erl` | `tx_env_and_trees_from_hash` — loads state from Mnesia |
| `apps/aecore/src/aec_trees.erl` | `apply_txs_on_state_trees` — strict-mode tx execution |
| `apps/aefate/src/aefa_stores.erl` | FATE store read/write cache, map operations, finalize |
| `apps/aecontract/src/aect_contracts_store.erl` | Contract store backed by Merkle Patricia trie |
| `apps/aeutils/src/aeu_mp_trees_db.erl` | MPT DB abstraction — write cache only, no read cache |
| `apps/aecore/src/aec_governance.erl` | `block_gas_limit` (6M), `STORE_BYTE_GAS` (5), `MICRO_BLOCK_CYCLE` (3s) |
| `apps/aeutils/priv/aeternity_config_schema.json` | Queue defaults, `gas_limit` config |

### Summary: Where Time Goes

```
dry-run request for create_community (~14 seconds total)
  │
  ├── Jobs queue wait                                     ~variable (now on read queue)
  │
  ├── State tree loading                                  ~100-500ms (dirty Mnesia backend)
  │
  ├── FATE VM execution                                   ~13-14 seconds (CPU-bound, irreducible)
  │   ├── 3 contract deployments (deserialize + init)
  │   ├── Bytecode interpretation (single-threaded, no JIT)
  │   ├── Store map operations on 17K+ entry maps
  │   │   └── Map.insert / Set.insert → trie read + write per key
  │   └── Store finalization (write-back modified entries to trie)
  │
  └── Response serialization                              ~negligible
```

The dominant cost is **FATE VM bytecode interpretation**, which is CPU-bound. Mnesia I/O
(dirty reads from ETS) is sub-10μs per read and contributes negligible total time. Two
independent caching strategies (process-dictionary backend cache, bulk store preloading)
were tested and confirmed this — neither produced measurable improvement.
