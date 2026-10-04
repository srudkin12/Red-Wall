#!/usr/bin/env python3
from pathlib import Path
import argparse,re,sys
def main():
    ap=argparse.ArgumentParser();ap.add_argument("root",nargs="?");a=ap.parse_args()
    root=Path(a.root).resolve() if a.root else Path(__file__).resolve().parents[1]
    rd=(root/"framework/R/TDABMRadiusDiagnostics.R").read_text(encoding="utf-8")
    urc=(root/"framework/R/TDABMUserRadiusContract.R").read_text(encoding="utf-8")
    ct=(root/"tools/test_p1_4_c_topology_contracts.R").read_text(encoding="utf-8")
    pt=(root/"tools/test_radius_policy_nonbinding_contract.R").read_text(encoding="utf-8")
    f=[]
    for tok in ["majority_cover_transition","max_ball_share_observations","effective","tdabm_rd_cpp_landmark_positions_one_based","TECHNICAL_EVIDENCE_COMPLETE_USER_DECISION_EXTERNAL","diagnostic_flags","integrity_checks"]:
        if tok.lower() not in rd.lower(): f.append("missing radius-policy marker: "+tok)
    if "TECHNICAL_APPROVAL_READY" in rd: f.append("technical diagnostics still use approval-ready status language")
    if "USER_REVIEW_REQUIRED" in rd: f.append("technical thresholds still collapse into USER_REVIEW_REQUIRED")
    if "permutation[as.integer(bm$landmarks)]" in rd: f.append("unsafe raw-landmark indexing remains")
    if "tdabm_urc_review_band_position" not in urc: f.append("user-radius contract does not record review-band position")
    if "approved radius must lie inside the recorded review band" in urc: f.append("binding review-band message remains")
    line=next((x for x in urc.splitlines() if "review_band_valid <-" in x),"")
    if not line: f.append("review_band_valid assignment missing")
    elif re.search(r'\bradius\b',line): f.append("review_band_valid still depends on approved-radius location")
    for tok in ["BELOW_REVIEW_BAND","ABOVE_REVIEW_BAND"]:
        if tok not in ct or tok not in pt: f.append("outside-band semantic regression missing: "+tok)
    for tok in ["severe_dominance_marker_crossed","blocking==FALSE","technical_evidence_complete"]:
        if tok not in pt: f.append("non-binding technical-review regression missing: "+tok)
    print("TDABM clean radius-policy check");print(f"Violations: {len(f)}")
    for x in f: print("FAIL:",x)
    if not f:
        print("PASS: 40%-60% review-band location is recorded but is not an admissibility predicate.")
        print("PASS: technical integrity checks are separated from non-blocking substantive diagnostic flags.")
        print("PASS: semantic regressions cover explicit approval below, inside and above the review band.")
    return 1 if f else 0
if __name__=="__main__":sys.exit(main())
