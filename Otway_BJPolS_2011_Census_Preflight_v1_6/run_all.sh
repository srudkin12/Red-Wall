#!/usr/bin/env bash
set -u

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE"
mkdir -p outputs/logs handback
STAMP="$(date +%Y%m%d_%H%M%S)"
LOG="outputs/logs/run_${STAMP}.log"
STATUS_FILE="outputs/RUN_STATUS.txt"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript is not available on PATH." | tee "$LOG"
  printf 'exit_code=127\nstatus=Rscript_missing\n' > "$STATUS_FILE"
  RC=127
else
  set +e
  Rscript --vanilla run_2011_census_preflight_v1_6.R 2>&1 | tee "$LOG"
  RC=${PIPESTATUS[0]}
  set -e
  if [ "$RC" -eq 0 ]; then
    printf 'exit_code=0\nstatus=completed\n' > "$STATUS_FILE"
  else
    printf 'exit_code=%s\nstatus=R_failed_but_handback_created\n' "$RC" > "$STATUS_FILE"
  fi
fi

ZIP="handback/Otway_BJPolS_2011_Census_Preflight_v1_6_Handback_${STAMP}.zip"
TMP_LIST="outputs/.handback_filelist_${STAMP}.txt"
find outputs -type f ! -name '.handback_filelist_*' -print | sort > "$TMP_LIST"
printf '%s\n' README.md SOURCE_AND_HARMONISATION_NOTES.md V1_1_FAILURE_AND_V1_2_REPAIR.md V1_2_FAILURE_AND_V1_3_REPAIR.md V1_3_FAIL_AND_V1_4_MAPPING_REPAIR.md V1_4_AMBER_REVIEW_AND_V1_5_RESOLUTION.md V1_5_PASS_AND_V1_6_DENSITY_UNIT_FREEZE_REPAIR.md run_2011_census_preflight_v1_6.R run_all.sh >> "$TMP_LIST"

if command -v zip >/dev/null 2>&1; then
  zip -q "$ZIP" -@ < "$TMP_LIST"
else
  echo "zip_command_missing=TRUE" >> "$STATUS_FILE"
  echo "ERROR: system 'zip' command is missing; outputs exist but ZIP could not be created." | tee -a "$LOG"
  rm -f "$TMP_LIST"
  exit 126
fi
rm -f "$TMP_LIST"

if command -v sha256sum >/dev/null 2>&1; then
  sha256sum "$ZIP" > "${ZIP}.sha256"
fi

echo
echo "HAND BACK ZIP: $HERE/$ZIP"
if [ -f "${ZIP}.sha256" ]; then
  echo "SHA256 SIDECAR: $HERE/${ZIP}.sha256"
fi
echo "R exit code: $RC"
exit "$RC"
