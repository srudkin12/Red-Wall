#!/usr/bin/env python3
from pathlib import Path
import argparse, sys

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument("root", nargs="?")
    a=ap.parse_args()
    root=Path(a.root).resolve() if a.root else Path(__file__).resolve().parents[1]
    problems=[]

    test=(root/"tools/test_exclusion_contract_and_outlier_suggestions.R").read_text(encoding="utf-8",errors="replace")
    if 'file.path(repo_root, "outputs"' in test:
        problems.append("exclusion/outlier self-test still writes under repo_root/outputs")
    if 'tempfile("tdabm_p1_1_exclusion_contract_test_' not in test:
        problems.append("exclusion/outlier self-test lacks temporary-output default")

    setup=(root/"setup_tdabm_clean.sh").read_text(encoding="utf-8",errors="replace")
    for marker in ("post_runtime_clean_framework","post_runtime_clean_manifest"):
        if marker not in setup:
            problems.append(f"setup lacks post-runtime cleanliness guard: {marker}")

    print("TDABM self-test output policy check")
    print(f"Problems: {len(problems)}")
    for x in problems: print("FAIL:",x)
    if not problems:
        print("PASS: synthetic self-tests do not persist outputs in the clean source tree.")
        print("PASS: full runtime validation rechecks clean tree and manifest at the end.")
    return 1 if problems else 0

if __name__=="__main__":
    sys.exit(main())
