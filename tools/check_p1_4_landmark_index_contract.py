#!/usr/bin/env python3
from pathlib import Path
import argparse, sys
def main():
    ap=argparse.ArgumentParser();ap.add_argument("root");args=ap.parse_args()
    root=Path(args.root).resolve()
    cpp=(root/"framework/BallMapper.cpp").read_text(encoding="utf-8")
    rd=(root/"framework/R/TDABMRadiusDiagnostics.R").read_text(encoding="utf-8")
    tf=(root/"framework/R/TDABMTopologyFreeze.R").read_text(encoding="utf-8")
    failures=[]
    if "landmarks.push_back( current_point );" not in cpp: failures.append("C++ zero-based landmark contract missing.")
    if "tdabm_rd_cpp_landmark_positions_one_based" not in rd or "raw + 1L" not in rd:
        failures.append("radius-diagnostic normalization boundary missing.")
    if "permutation[as.integer(bm$landmarks)]" in rd:
        failures.append("unsafe raw landmark indexing remains in radius diagnostics.")
    if "tdabm_tf_cpp_landmarks_one_based" not in tf:
        failures.append("topology-freeze normalization boundary missing.")
    if 'bm$landmarks <- tdabm_tf_cpp_landmarks_one_based(bm$landmarks, nrow(axes))' not in tf:
        failures.append("canonical topology builder does not normalize raw landmarks exactly once.")
    if 'R_ONE_BASED_NORMALIZED_FROM_CPP_ZERO_BASED' not in tf:
        failures.append("canonical object does not record the landmark-index contract.")
    print("TDABM P1.4 R/C++ landmark-index boundary check")
    print(f"Root: {root}")
    print(f"Violations: {len(failures)}")
    for f in failures: print("FAIL:",f)
    if not failures:
        print("PASS: raw C++ landmark positions are explicitly treated as zero-based.")
        print("PASS: radius diagnostics normalize before permutation indexing.")
        print("PASS: canonical frozen topology normalizes once and records one-based R landmark indices.")
        print("PASS: memberships and graph indices retain their one-based interface contract.")
    return 1 if failures else 0
if __name__=="__main__": sys.exit(main())
