#!/usr/bin/env bash
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE"

STAMP="$(date +%Y%m%d_%H%M%S)"
mkdir -p outputs/logs

echo "===== OTWAY BJPolS TARGETED EMPIRICAL ADDENDUM v1.2 ====="
echo "Started: $(date -Is)"
echo "Package: $HERE"
echo

Rscript run_targeted_addendum_v1_2.R 2>&1 | tee "outputs/logs/run_${STAMP}.log"

echo
echo "===== FINISHED ====="
echo "Finished: $(date -Is)"
echo
echo "Handback files:"
find outputs/handback -maxdepth 1 -type f -printf '%p\n' 2>/dev/null | sort || true
