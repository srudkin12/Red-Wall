#!/usr/bin/env bash
set -Eeuo pipefail
APP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ROOT="$(cd "$APP_DIR/../.." && pwd)"
APP_ID="$(basename "$APP_DIR")"
TS="$(date +%Y%m%d_%H%M%S)"
RUN="$APP_DIR/runs/p1_4_a_$TS"
AUDIT="$RUN/${APP_ID}_P1_4_A_Audit_$TS"
EXPORT="$APP_DIR/exports/${APP_ID}_P1_4_A_Audit_$TS.zip"
mkdir -p "$AUDIT" "$APP_DIR/exports"

Rscript --vanilla "$APP_DIR/scripts/01_build_p1_4_a.R" \
  --repo="$ROOT" --app-dir="$APP_DIR" --audit-dir="$AUDIT" \
  2>&1 | tee "$AUDIT/P1_4_A_run.log"

grep -Fq "${APP_ID} P1.4-A: PASS" "$AUDIT/P1_4_A_ACCEPTANCE.txt"
grep -Fq "READY_FOR_P1_4_B=YES" "$AUDIT/P1_4_A_ACCEPTANCE.txt"

(
  cd "$RUN"
  zip -qr "$EXPORT" "$(basename "$AUDIT")"
)
(
  cd "$(dirname "$EXPORT")"
  sha256sum "$(basename "$EXPORT")" > "$(basename "$EXPORT").sha256"
)

echo
echo "P1.4-A complete:"
echo "  $EXPORT"
echo "  $EXPORT.sha256"
