#!/usr/bin/env bash
set -Eeuo pipefail
MODE="${1:-production}"
[[ "$MODE" == "production" || "$MODE" == "preflight" ]] || { echo "ERROR: mode must be production or preflight"; exit 64; }

APP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ROOT="$(cd "$APP_DIR/../.." && pwd)"
APP_ID="$(basename "$APP_DIR")"
TS="$(date +%Y%m%d_%H%M%S)"
RUN="$APP_DIR/runs/p1_4_b_${MODE}_$TS"
AUDIT="$RUN/${APP_ID}_P1_4_B_${MODE}_Audit_$TS"
EXPORT="$APP_DIR/exports/${APP_ID}_P1_4_B_${MODE}_Audit_$TS.zip"
mkdir -p "$AUDIT" "$APP_DIR/exports"

Rscript --vanilla "$APP_DIR/scripts/02_run_p1_4_b_radius_diagnostics.R" \
  --repo="$ROOT" --app-dir="$APP_DIR" --audit-dir="$AUDIT" --mode="$MODE" \
  2>&1 | tee "$AUDIT/P1_4_B_run.log"

grep -Fq "${APP_ID} P1.4-B ${MODE^^}: PASS" "$AUDIT/P1_4_B_ACCEPTANCE.txt"

(
  cd "$RUN"
  zip -qr "$EXPORT" "$(basename "$AUDIT")"
)
(
  cd "$(dirname "$EXPORT")"
  sha256sum "$(basename "$EXPORT")" > "$(basename "$EXPORT").sha256"
)

echo
echo "P1.4-B $MODE complete:"
echo "  $EXPORT"
echo "  $EXPORT.sha256"
