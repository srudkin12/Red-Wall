#!/usr/bin/env bash
set -Eeuo pipefail

APP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ROOT="$(cd "$APP_DIR/../.." && pwd)"
APP_ID="$(basename "$APP_DIR")"
TS="$(date +%Y%m%d_%H%M%S)"

DECISION="$APP_DIR/config/p1_4_c_radius_decision.csv"
[[ -f "$DECISION" ]] || { echo "ERROR: missing $DECISION" >&2; exit 2; }

B_NAME="$(awk -F, '$1=="diagnostic_audit_filename"{gsub(/^"|"$/,"",$2); print $2}' "$DECISION")"
B_EXPECTED_SHA="$(awk -F, '$1=="diagnostic_audit_sha256"{gsub(/^"|"$/,"",$2); print $2}' "$DECISION")"
B_ZIP_DEFAULT="$APP_DIR/exports/$B_NAME"
B_ZIP="${TDABM_P1_4_C_B_AUDIT_ZIP:-$B_ZIP_DEFAULT}"

[[ -f "$B_ZIP" ]] || {
  echo "ERROR: accepted P1.4-B production ZIP not found:" >&2
  echo "  $B_ZIP" >&2
  echo "Set TDABM_P1_4_C_B_AUDIT_ZIP=/path/to/the/accepted/zip if it was moved." >&2
  exit 3
}

OBSERVED_SHA="$(sha256sum "$B_ZIP" | awk '{print $1}')"
[[ "$OBSERVED_SHA" == "$B_EXPECTED_SHA" ]] || {
  echo "ERROR: P1.4-B audit SHA mismatch." >&2
  echo "expected: $B_EXPECTED_SHA" >&2
  echo "observed: $OBSERVED_SHA" >&2
  exit 4
}

RUN="$APP_DIR/runs/p1_4_c_$TS"
AUDIT="$RUN/${APP_ID}_P1_4_C_Audit_$TS"
FREEZE="$APP_DIR/frozen_topology/p1_4_c_v1_0"
EXPORT="$APP_DIR/exports/${APP_ID}_P1_4_C_Audit_$TS.zip"

[[ ! -e "$FREEZE" ]] || {
  echo "ERROR: canonical P1.4-C freeze already exists; refusing to overwrite:" >&2
  echo "  $FREEZE" >&2
  exit 5
}

mkdir -p "$AUDIT" "$APP_DIR/exports"

Rscript --vanilla "$APP_DIR/scripts/03_build_p1_4_c.R" \
  --repo="$ROOT" \
  --app-dir="$APP_DIR" \
  --audit-dir="$AUDIT" \
  --freeze-dir="$FREEZE" \
  --b-audit-zip="$B_ZIP" \
  2>&1 | tee "$AUDIT/P1_4_C_run.log"

grep -Fq "${APP_ID} P1.4-C: PASS" "$AUDIT/P1_4_C_ACCEPTANCE.txt"
grep -Fq "READY_FOR_P1_4_D=YES" "$AUDIT/P1_4_C_ACCEPTANCE.txt"

# Freeze files are read-only downstream inputs.
find "$FREEZE" -type f -exec chmod 0444 {} +
find "$FREEZE" -type d -exec chmod 0555 {} +

(
  cd "$RUN"
  zip -qr "$EXPORT" "$(basename "$AUDIT")"
)
(
  cd "$(dirname "$EXPORT")"
  sha256sum "$(basename "$EXPORT")" > "$(basename "$EXPORT").sha256"
)

echo
echo "P1.4-C complete:"
echo "  canonical freeze: $FREEZE"
echo "  audit ZIP       : $EXPORT"
echo "  checksum        : $EXPORT.sha256"
