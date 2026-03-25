# Zero-Block Issue: Debugging Analysis

## Problem

When calling `create_community` on the `CommunityFactory` contract (`ct_25cqTw85wkF5cbcozmHHUCuybnfH9WaRZXSgEcNNXG9LsCJWTN`), the network enters a state where key blocks are produced but contain **zero transactions** (no microblocks) for an extended period.

## Why `create_community` Is Expensive

The function deploys **3 new contracts** and makes multiple cross-contract calls in a single transaction:

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

This makes it one of the most gas/compute-intensive transactions on mainnet.

## Root Cause

The block production pipeline has a single-worker design that gets starved by long-running transactions.

### Block Production Pipeline

The microblock generator (`aec_block_generator.erl`) uses a **single worker process** to build candidate microblocks:

```
apps/aecore/src/aec_block_generator.erl (line 233-236)

start_worker_block(S = #state{ worker = undefined }, BlockOrBlockHash) ->
    {Pid, Ref} = spawn_monitor(fun() -> create_block_candidate(BlockOrBlockHash) end),
    S#state{ worker = {Pid, Ref}, new_txs = [], candidate = undefined }.
```

The worker calls `aec_block_micro_candidate:create()` which pulls transactions from the pool and executes them **synchronously one by one** via `int_apply_block_txs`. There is **no per-transaction execution timeout**.

### The Preemption Kill

When a new key block arrives, the current worker is **killed and restarted**:

```
apps/aecore/src/aec_block_generator.erl (line 195-201)

preempt_generation(S, #{ block_hash := NewTop }) ->
    S1 = stop_worker(S),
    start_worker_block(S1, NewTop).

stop_worker(S = #state{ worker = {WPid, WRef} }) ->
    erlang:demonitor(WRef, [flush]),
    erlang:exit(WPid, finished),
    S#state{ worker = undefined }.
```

### The Starvation Loop

When `create_community` execution time exceeds the key block interval (~3 minutes on mainnet):

```
1. Worker starts
   → pulls create_community tx from pool
   → begins long execution

2. New key block arrives (top_changed event)
   → worker killed mid-execution
   → candidate discarded (no microblock published)

3. New worker starts on new top
   → pulls same create_community tx from pool (still in mempool)
   → begins long execution again

4. Another key block arrives
   → worker killed again
   → repeat

Result: zero-transaction blocks indefinitely
```

While the worker is busy with the heavy tx, all other transactions are blocked too:
- New txs are cached in `new_txs` (line 190) but can't be processed
- No microblock candidate is ever published because the worker never finishes
- Empty candidates (0 txs) are explicitly **not** published (line 111-114):

```
apps/aecore/src/aec_block_generator.erl (line 108-117)

handle_cast({worker_done, Pid, {candidate, Candidate, CandidateState}},
            State = #state{ worker = {Pid, _} }) ->
    case aec_blocks:txs(Candidate) of
        [] ->
            lager:debug("Empty microblock candidate prepared", []),
            ok;
        _  ->
            epoch_mining:info("New microblock candidate ready", []),
            publish_candidate(Candidate)
    end,
```

### Why It Eventually Resolves

- The tx expires from the mempool (TTL)
- Network difficulty shifts so key blocks come slower, giving the worker enough time
- The tx lands during a lucky gap between key blocks

## Key Files

| File | Role |
|------|------|
| `apps/aecore/src/aec_block_generator.erl` | Single-worker microblock candidate builder |
| `apps/aecore/src/aec_block_micro_candidate.erl` | Pulls txs from pool, executes them, builds microblock |
| `apps/aecore/src/aec_trees.erl` | `apply_txs_on_state_trees` — synchronous tx execution |
| `apps/aecore/src/aec_conductor.erl` | Orchestrates key block mining and microblock signing |
| `apps/aecore/src/aec_governance.erl` | `block_gas_limit` (6M), `micro_block_cycle` (3s) |

## Additional Root Cause Detail: Micro Block Preemption

A deeper reading of the code reveals the starvation is **worse than initially described**.

The `top_changed` event fires for **both** key blocks AND micro blocks. The block generator's
`preempt_generation` does **not** distinguish between them — it kills the worker on every
`top_changed` regardless of block type:

```
apps/aecore/src/aec_block_generator.erl (line 136-146)

handle_info({gproc_ps_event, Event, #{info := Info}}, State) ->
    State1 =
        case Event of
            top_changed   -> preempt_generation(State, Info);  %% kills worker on ANY top change
            ...
        end,
```

On mainnet, micro blocks from the current leader arrive roughly every **3 seconds**
(`MICRO_BLOCK_CYCLE = 3000ms`). When we receive a micro block from the current leader
(or from a competing fork), the worker building our candidate gets killed.

This means during active block production, the worker may have as little as **3 seconds**
to complete before being killed — far less than the ~3 minute key block interval.

The `aec_conductor.erl` already makes this distinction for key block candidate mining
(line 966-978): it does NOT reset key block mining when a micro block arrives from the
same generation. But the block generator was not given the same treatment.

## Applied Fix: Micro-Block-Tolerant Worker

### Design

**File**: `apps/aecore/src/aec_block_generator.erl`

**Principle**: Only kill the worker on **key block** top changes (new generation). When a
**micro block** top change arrives while the worker is running, let the worker finish and
track the latest known top. When the worker completes, check if the candidate was built on
the current top — if not, rebuild.

This is safe because:
1. This change is **non-consensus**: it only affects how the local node builds candidates,
   not what blocks are considered valid by the network. No hard fork required.
2. Key block preemption is preserved: new generations always restart the worker.
3. Stale candidates are detected and discarded: if micro blocks moved the top while the
   worker was running, the finished candidate is rebuilt on the correct top.
4. When we are the leader, we only receive micro block `top_changed` events from our own
   published micro blocks (which only happen AFTER the worker finishes). So in the normal
   leader case, the new code path is rarely exercised. It primarily helps when receiving
   micro blocks from competing forks during candidate building.

### State Changes

Add a `pending_top` field to the `#state{}` record. This holds the latest micro block top
hash that arrived while the worker was running:

- `undefined` — no deferred top change, worker's candidate is current
- `{pending, BlockHash}` — a micro block moved the top while the worker was busy

### Event Handling Changes

Replace the blanket `top_changed -> preempt_generation(State, Info)` with:

| Block type | Worker running? | Action |
|------------|-----------------|--------|
| key        | any             | **Preempt**: kill worker, restart on new top (new generation) |
| micro      | yes             | **Defer**: record new top in `pending_top`, let worker finish |
| micro      | no              | **Preempt**: rebuild candidate on new top (normal flow) |

### Worker Completion Changes

When the worker finishes (`worker_done` with `candidate`), check `pending_top`:

- If `undefined`: publish the candidate as before (it's current).
- If `{pending, NewTop}`: discard the stale candidate, start a fresh worker on `NewTop`.

This ensures we never publish a candidate built on an outdated parent.

### Why This Resolves the Zero-Block Issue

Before the fix, a `create_community` tx taking 10+ seconds would be killed every ~3 seconds
by micro block `top_changed` events. The worker could never finish, so no micro blocks were
produced, and the expensive tx blocked all other transactions.

After the fix, the worker runs uninterrupted for the full key block interval (~3 minutes).
Even a very expensive tx (large state serialization for 17K+ entries) has ample time to
complete. Other transactions in the same candidate batch also get through.

## Other Possible Fixes (Not Applied)

### Per-Transaction Execution Timeout

Add a time limit when executing individual transactions in `int_pack_block`. If a tx exceeds
the timeout, skip it and continue with other txs in the pool.

**Where**: `aec_block_micro_candidate.erl`, `int_pack_block/8` and `aec_trees.erl`, `tx_process/4`

**Tradeoff**: Cannot interrupt a running Erlang function without killing the process. Would
require spawning each tx in a monitored subprocess with a timer, adding complexity. Also
doesn't help if the expensive tx is legitimately within gas limits — it should eventually
be included.

### Transaction Deprioritization

Track how many times a tx has been attempted but preempted. After N attempts, move it to the
back of the queue so other txs can go through.

**Where**: `aec_tx_pool.erl`, `get_candidate/3`

**Tradeoff**: May delay legitimate expensive txs even further, but unblocks the rest of the
network. The existing `failures` counter only tracks actual execution failures, not
preemptions.

## Contract Reference

- **Contract**: `ct_25cqTw85wkF5cbcozmHHUCuybnfH9WaRZXSgEcNNXG9LsCJWTN`
- **Source**: https://github.com/bctsl/bctsl-contracts/blob/main/contracts/CommunityFactory.aes
- **Explorer**: https://aescan.io/contracts/ct_25cqTw85wkF5cbcozmHHUCuybnfH9WaRZXSgEcNNXG9LsCJWTN
- **Function**: `create_community` — over 17,276 tokens created
