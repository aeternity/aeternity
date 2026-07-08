#!/usr/bin/env bash
# Freeze gate: fails if apps/aefate/src/aefa_stores_ceres.erl has drifted from
# its committed golden hash. Dependency-free twin of the eunit test
# aefa_stores_ceres_freeze_test.erl (the authoritative version).
#
# Usage:
#   ./check_stores_ceres_freeze.sh          # verify (0 = OK, 1 = drifted)
#   ./check_stores_ceres_freeze.sh --print  # print the normalized sha256
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
CERES="${REPO_ROOT}/apps/aefate/src/aefa_stores_ceres.erl"

# Keep in lock-step with aefa_stores_ceres_freeze_test.erl.
GOLDEN_SHA256="6aef651ad2dd45f83342ea07f87ce2f1f82d95a1b6c8411674cfaf4b8ab42f93"

if [ ! -f "${CERES}" ]; then
    echo "check_stores_ceres_freeze: ${CERES} not found" >&2
    exit 2
fi

normalized_sha256() {
    # Strip the -module(...). line and any "%% FROZEN" banner, hash the rest.
    grep -Ev '^-module\(.+\)\.[[:space:]]*$' "$1" \
        | grep -Ev '^[[:space:]]*%% FROZEN' \
        | sha256sum \
        | awk '{print $1}'
}

ACTUAL_SHA256="$(normalized_sha256 "${CERES}")"

if [ "${1:-}" = "--print" ]; then
    echo "${ACTUAL_SHA256}"
    exit 0
fi

if [ "${ACTUAL_SHA256}" != "${GOLDEN_SHA256}" ]; then
    echo "FREEZE VIOLATION: apps/aefate/src/aefa_stores_ceres.erl has changed" >&2
    echo "  since its freeze (normalized sha256 mismatch)." >&2
    echo "  expected (golden): ${GOLDEN_SHA256}" >&2
    echo "  actual (current):  ${ACTUAL_SHA256}" >&2
    echo "" >&2
    echo "  aefa_stores_ceres.erl is the frozen Iris..Ceres store reference and" >&2
    echo "  must not be edited. To deliberately re-baseline, see the eunit test." >&2
    exit 1
fi

echo "OK: apps/aefate/src/aefa_stores_ceres.erl matches its frozen golden."
exit 0
