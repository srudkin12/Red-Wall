#!/usr/bin/env bash
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE"
mkdir -p outputs/logs handback
STAMP="$(date +%Y%m%d_%H%M%S)"
LOG="outputs/logs/run_${STAMP}.log"

set +e
Rscript run_temporal_concentration_contrast_v1_0.R 2>&1 | tee "$LOG"
RC=${PIPESTATUS[0]}
set -e

{
  echo "r_exit_code=${RC}"
  echo "timestamp=${STAMP}"
  echo "package=Otway_BJPolS_2011_2021_Temporal_Concentration_Contrast_v1_0"
} > outputs/SHELL_RUN_STATUS.txt

HB="handback/Otway_BJPolS_2011_2021_Temporal_Concentration_Contrast_v1_0_Handback_${STAMP}.zip"
zip -qr "$HB" outputs input README.md PROVENANCE.md \
  run_temporal_concentration_contrast_v1_0.R run_all.sh

if command -v sha256sum >/dev/null 2>&1; then
  sha256sum "$HB" > "${HB}.sha256"
fi

echo
echo "HAND BACK ZIP: $HERE/$HB"
if [ -f "${HB}.sha256" ]; then
  echo "SHA256 SIDECAR: $HERE/${HB}.sha256"
fi
echo "R exit code: $RC"
exit "$RC"
