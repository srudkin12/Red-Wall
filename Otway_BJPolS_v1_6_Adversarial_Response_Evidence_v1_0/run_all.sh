#!/usr/bin/env bash
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
cd "$HERE"
mkdir -p outputs/logs
python3 scripts/run_v1_6_evidence.py
STAMP="$(date +%Y%m%d_%H%M%S)"
HAND="Otway_BJPolS_v1_6_Adversarial_Response_Evidence_v1_0_Handback_${STAMP}.zip"
# Hash source/input/output files before packaging.
find README.md PROVENANCE.md run_all.sh scripts input outputs -type f ! -name '*.zip' ! -name '*.sha256' -print0 | sort -z | xargs -0 sha256sum > outputs/HANDBACK_MANIFEST_SHA256.txt
zip -qr "$HAND" README.md PROVENANCE.md run_all.sh scripts input outputs
sha256sum "$HAND" > "${HAND}.sha256"
echo "Created $HAND"
echo "Created ${HAND}.sha256"
