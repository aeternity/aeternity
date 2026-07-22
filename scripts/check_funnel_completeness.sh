#!/usr/bin/env bash
#
# Funnel-completeness guard.
#
# Every consensus read of a batched object kind must go through its
# `*_state_tree` funnel (which honours the per-microblock batch +
# tombstones).  Reaching a consensus sub-tree directly bypasses the
# batch and would read stale / resurrect deleted state.
#
# Two regression detectors (the equivalence tests + protocol suites are
# the actual proof):
#
#   Check 1 (strict): raw `aeu_mtrees:lookup/get` on a consensus
#     sub-tree is allowed ONLY from the funnel-owner modules.
#
#   Check 2 (regression): raw sub-tree enumeration
#     (`aeu_mtrees:to_list|iterator|iterator_from|fold`) or the lower
#     `aeu_mp_trees:` API is allowed from the funnel owners plus an
#     audited set that provably operates on flushed/historic trees or
#     is MPT/DB/sync internals.  A NEW module appearing here must be
#     routed through a funnel, or added to the allowlist WITH its
#     reason after review.
#
# Exits non-zero on any module outside the relevant allowlist.

set -euo pipefail

cd "$(dirname "$0")/.."

# Funnel owners: each owns its kind's batch + tombstone-aware funnel.
OWNERS="aec_accounts_trees aect_state_tree aect_call_state_tree \
aeo_state_tree aens_state_tree aesc_state_tree"

# Audited safe for enumeration / low-level access: NOT mid-microblock
# consensus reads.
#   aec_chain_state    - folds the flushing aect_call_state_tree:iterator/1
#   aec_poi            - proofs are built after add_poi flushes the batch
#   aect_contracts_store - contract-store sub-overlay (funnel internal)
#   aec_db aec_db_gc aec_tx_pool_sync aeu_mp_trees_db aeu_mtrees
#                      - DB / sync / MPT implementation internals
AUDITED="aec_chain_state aec_poi aect_contracts_store \
aec_db aec_db_gc aec_tx_pool_sync aeu_mp_trees_db aeu_mtrees"

# check <label> <regex> <allowlist...>
check() {
  local label="$1"; shift
  local regex="$1"; shift
  local allow=" $* "
  local offenders=()
  local mod
  while IFS= read -r mod; do
    [[ -z "$mod" ]] && continue
    [[ "$allow" == *" $mod "* ]] || offenders+=("$mod")
  done < <(
    grep -rlE --include='*.erl' "$regex" apps/*/src 2>/dev/null \
      | xargs -r -n1 basename | sed 's/\.erl$//' | sort -u)

  if [[ ${#offenders[@]} -gt 0 ]]; then
    echo "FUNNEL GUARD FAILED (${label}): these modules reach a consensus"
    echo "sub-tree outside the allowlist:"
    printf '  - %s\n' "${offenders[@]}"
    echo
    echo "Route the access through the kind's *_state_tree funnel, or —"
    echo "only if it provably runs on flushed/historic trees — add the"
    echo "module to the ${label} allowlist WITH its reason."
    exit 1
  fi
  echo "  OK ${label}"
}

echo "funnel-completeness:"
check "lookup/get"   'aeu_mtrees:(lookup|get)\('                         $OWNERS
check "enumerate/mp" 'aeu_mtrees:(to_list|iterator|iterator_from|fold)\(|aeu_mp_trees:' \
                                                                        $OWNERS $AUDITED
echo "funnel-completeness OK"
