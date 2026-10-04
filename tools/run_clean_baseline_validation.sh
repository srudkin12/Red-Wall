#!/usr/bin/env bash
set -Eeuo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"; TS="$(date +%Y%m%d_%H%M%S)"; OUTDIR="${1:-$(dirname "$ROOT")/TDABM_Clean_Validation_$TS}"; mkdir -p "$OUTDIR"
ENVFILE="$OUTDIR/environment.txt"; LOG="$OUTDIR/full_selftest.log"; REGISTER="$OUTDIR/clean_baseline_test_register.csv"; EVIDENCE="$OUTDIR/TDABM_Clean_Baseline_Full_Validation_v1_0.md"
{
 echo "timestamp=$(date --iso-8601=seconds 2>/dev/null || date)"; echo "command=bash tools/run_clean_baseline_validation.sh"; echo "root=$ROOT"; echo "uname=$(uname -a)"; echo "python=$(python3 --version 2>&1)"
 if command -v Rscript >/dev/null 2>&1; then echo "Rscript=$(Rscript --version 2>&1 | head -1)"; Rscript --vanilla - <<'RS'
cat('R.version.string=',R.version.string,'\n',sep=''); pkgs<-c('BallMapper','Rcpp','data.table','doSNOW','dplyr','fields','foreach','ggplot2','igraph','png','digest'); for(p in pkgs){v<-if(requireNamespace(p,quietly=TRUE))as.character(utils::packageVersion(p))else'NOT_INSTALLED'; cat('package.',p,'=',v,'\n',sep='')}
RS
 else echo 'Rscript=NOT_AVAILABLE'; fi
} > "$ENVFILE"
set +e
python3 "$ROOT/tools/test_clean_setup_status_semantics.py" "$ROOT" 2>&1 | tee "$LOG"
STATUS_RC=${PIPESTATUS[0]}
set -e
if [[ "$STATUS_RC" -ne 0 ]]; then
  echo "FULL CLEAN-BASELINE VALIDATION: FAIL"
  echo "ERROR: setup status-semantics regression failed."
  exit "$STATUS_RC"
fi
echo "TDABM_TEST|setup_status_semantics_regression|PASS" | tee -a "$LOG"
set +e
bash "$ROOT/setup_tdabm_clean.sh" 2>&1 | tee -a "$LOG"
RC=${PIPESTATUS[0]}
set -e
python3 - "$LOG" "$REGISTER" "$EVIDENCE" "$ENVFILE" "$RC" <<'PYEVIDENCE'
from pathlib import Path
import csv,sys
log=Path(sys.argv[1]).read_text(encoding='utf-8',errors='replace'); reg=Path(sys.argv[2]); ev=Path(sys.argv[3]); env=Path(sys.argv[4]).read_text(encoding='utf-8',errors='replace').strip(); rc=int(sys.argv[5])
req=[('static_clean_framework','Static clean/application-neutral framework checks'),('static_current_version_consistency','Current-baseline version consistency'),('static_radius_policy','Radius-policy static checks'),('static_packed_signature','Packed-signature static contract'),('static_landmark_index','C++/R landmark-index contract'),('static_namespace','Namespace checks'),('static_data_table_mutation','data.table mutation checks'),('static_dynamic_regex','Dynamic-regex checks'),('static_r_structure','R lexical/structure checks'),('clean_manifest','Clean manifest verification'),('setup_status_semantics_regression','Self-test status-semantics regression'),('r_dependencies','R dependency verification'),('r_framework_source_loading','Framework source-loading test'),('r_exclusion_outlier_contract','Exclusion/outlier contract tests'),('r_semantic_equivalence','Semantic-equivalence comparator tests'),('r_p1_4_a_input_contracts','P1.4-A tests'),('r_p1_4_b_radius_diagnostics','P1.4-B radius-diagnostic tests'),('r_radius_policy_nonbinding_contract','User-radius/non-binding regression tests'),('r_p1_4_c_topology_contracts','P1.4-C canonical-topology tests'),('r_p1_4_d_paper_evidence','P1.4-D paper-evidence tests'),('r_p1_4_d_interpretation_robustness','P1.4-D interpretation-robustness tests')]
rows=[]
for key,desc in req:
 marker=f'TDABM_TEST|{key}|PASS'; rows.append((key,desc,'PASS' if marker in log else 'NOT_CONFIRMED',marker))
complete=rc==0 and 'TDABM CLEAN BASELINE SELF-TEST: COMPLETE PASS' in log and all(x[2]=='PASS' for x in rows)
with reg.open('w',newline='',encoding='utf-8') as f:
 w=csv.writer(f); w.writerow(['test_id','description','status','evidence_marker']); w.writerows(rows)
lines=['# TDABM Clean Baseline — Full Validation Evidence v1.0','','## Environment','','```text',env,'```','','## Validation command','','```bash','bash tools/run_clean_baseline_validation.sh','```','','## Test results','','| Test | Result |','|---|---|']
for _,desc,status,_ in rows: lines.append(f'| {desc} | {status} |')
lines += ['', '## Final process status','',f'- exit status: `{rc}`',f"- all required test markers present: `{'YES' if all(x[2]=='PASS' for x in rows) else 'NO'}`",f"- final COMPLETE PASS marker present: `{'YES' if 'TDABM CLEAN BASELINE SELF-TEST: COMPLETE PASS' in log else 'NO'}`",'', '## Overall clean-baseline status','',f"**{'COMPLETE PASS' if complete else 'FAIL / INCOMPLETE'}**",'', 'The complete validation log and machine-readable register are retained in this evidence directory.']
ev.write_text('\n'.join(lines)+'\n',encoding='utf-8')
print('COMPLETE_PASS' if complete else 'INCOMPLETE')
PYEVIDENCE
echo "exit_status=$RC" > "$OUTDIR/exit_status.txt"
if [[ "$RC" -ne 0 ]]; then echo "FULL CLEAN-BASELINE VALIDATION: FAIL"; echo "Evidence: $EVIDENCE"; exit "$RC"; fi
grep -Fq '**COMPLETE PASS**' "$EVIDENCE"; grep -Fq 'TDABM CLEAN BASELINE SELF-TEST: COMPLETE PASS' "$LOG"; echo 'FULL CLEAN-BASELINE VALIDATION: PASS'; echo "Evidence: $EVIDENCE"; echo "Register: $REGISTER"; echo "Log: $LOG"
