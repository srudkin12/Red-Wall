#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT/data/user/merged_data.csv"
RW="$ROOT/data/user/redwall_source.csv"
EXPECTED_SHA="79ac20539ee5dc2db7ffae8496996151bc5d7c6a2dd245bf1d8a3f9e958f5643"

test -f "$SRC" || { echo "Missing $SRC" >&2; return 1 2>/dev/null || exit 1; }
test -f "$RW" || { echo "Missing $RW" >&2; return 1 2>/dev/null || exit 1; }

DEST="$ROOT/application_evidence/Otway_Rudkin_2024_v1_0/source"
mkdir -p "$DEST"
cp -f "$SRC" "$DEST/merged_data.csv"

OBSERVED_SHA="$(sha256sum "$SRC" | awk '{print $1}')"
if [[ "$OBSERVED_SHA" == "$EXPECTED_SHA" ]]; then
  export OTWAY_SOURCE_MODE="strict"
  echo "Source mode: strict historical source (SHA-256 matched)."
else
  export OTWAY_SOURCE_MODE="${OTWAY_SOURCE_MODE:-reconstructed}"
  echo "WARNING: merged_data.csv does not match the historical byte-level SHA-256." >&2
  echo "Running in OTWAY_SOURCE_MODE=$OTWAY_SOURCE_MODE." >&2
  echo "The public canonical builder will still require the accepted topology fingerprint" >&2
  echo "unless OTWAY_ALLOW_TOPOLOGY_VARIATION=1 is explicitly set." >&2
fi

export OTWAY_ROOT="$ROOT"
export OTWAY_RED_WALL_SOURCE="$RW"
export OTWAY_RED_WALL_AUDIT="$ROOT/Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1/outputs/03_redwall_source_match_audit.csv"

echo "OTWAY_ROOT=$OTWAY_ROOT"
echo "OTWAY_RED_WALL_SOURCE=$OTWAY_RED_WALL_SOURCE"
echo "OTWAY_RED_WALL_AUDIT=$OTWAY_RED_WALL_AUDIT"
echo "OTWAY_SOURCE_MODE=$OTWAY_SOURCE_MODE"
