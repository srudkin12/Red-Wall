#!/usr/bin/env bash
set -Eeuo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUTDIR="${1:-$(dirname "$ROOT")}"
VERSION="$(awk -F= '$1=="package_version"{print $2}' "$ROOT/VERSION")"
[[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] || { echo "ERROR: invalid package_version: $VERSION"; exit 1; }
TOKEN="${VERSION//./_}"
BASENAME="TDABM_Portability_Clean_Start_v${TOKEN}.zip"
OUT="$OUTDIR/$BASENAME"
SHA="$OUT.sha256"
bash "$ROOT/setup_tdabm_clean.sh"
mkdir -p "$OUTDIR"
rm -f "$OUT" "$SHA"
(
  cd "$(dirname "$ROOT")"
  zip -r -9 -q "$OUT" "$(basename "$ROOT")" \
    -x '*/applications/*/data/local/*' '*/config/local.R' '*/outputs/*' '*/.git/*' '*/__pycache__/*' '*/.fuse_hidden*'
)
unzip -tq "$OUT"
(
  cd "$OUTDIR"
  sha256sum "$BASENAME" > "$BASENAME.sha256"
)
python3 "$ROOT/tools/check_release_identity.py" "$ROOT" "$OUT" "$SHA"
echo "Created $OUT"
echo "Created $SHA"
