#!/usr/bin/env bash
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE"
mkdir -p outputs/logs handback
STAMP="$(date +%Y%m%d_%H%M%S)"
LOG="outputs/logs/run_${STAMP}.log"

choose_python() {
  if python3 - <<'PY' >/dev/null 2>&1
import numpy, pandas, matplotlib
PY
  then
    echo "python3"
    return
  fi
  echo "Required Python packages missing; creating local venv." >&2
  python3 -m venv .venv
  .venv/bin/python -m pip install --upgrade pip
  .venv/bin/python -m pip install -r requirements.txt
  echo ".venv/bin/python"
}

PY="$(choose_python)"
RC=0
set +e
{
  echo "===== FINAL FIGURE PRODUCTION ====="
  echo "Date: $(date)"
  echo "Python: $PY"
  echo
  "$PY" scripts/generate_final_figures.py
} 2>&1 | tee "$LOG"
RC=${PIPESTATUS[0]}
set -e

{
  echo "exit_code=${RC}"
  echo "timestamp=${STAMP}"
  echo "package=Otway_BJPolS_Final_Figure_Production_v1_1"
} > outputs/SHELL_RUN_STATUS.txt

HB="handback/Otway_BJPolS_Final_Figure_Production_v1_1_Handback_${STAMP}.zip"
zip -qr "$HB" outputs reference README.md FIGURE_GOVERNANCE.md \
  scripts requirements.txt run_all.sh

if command -v sha256sum >/dev/null 2>&1; then
  sha256sum "$HB" > "${HB}.sha256"
fi

echo
echo "HAND BACK ZIP: $HERE/$HB"
if [ -f "${HB}.sha256" ]; then
  echo "SHA256 SIDECAR: $HERE/${HB}.sha256"
fi
echo "Exit code: $RC"
exit "$RC"
