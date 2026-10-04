#!/usr/bin/env python3
from pathlib import Path
import argparse,os,subprocess,sys
def main():
    ap=argparse.ArgumentParser();ap.add_argument("root",nargs="?");a=ap.parse_args()
    root=Path(a.root).resolve() if a.root else Path(__file__).resolve().parents[1]
    setup=root/"setup_tdabm_clean.sh";env=os.environ.copy();env["TDABM_TEST_FORCE_R_UNAVAILABLE"]="1"
    s=subprocess.run(["bash",str(setup),"--static-only"],cwd=root,env=env,text=True,capture_output=True)
    if s.returncode!=0 or "STATIC-ONLY PASS / FULL VALIDATION NOT PERFORMED" not in s.stdout or "COMPLETE PASS" in s.stdout:
        print("FAIL: static-only status semantics");return 1
    f=subprocess.run(["bash",str(setup)],cwd=root,env=env,text=True,capture_output=True)
    if f.returncode==0 or "FAIL / FULL VALIDATION NOT PERFORMED" not in f.stdout or "COMPLETE PASS" in f.stdout:
        print("FAIL: full-mode missing-R status semantics");return 1
    print("PASS: static-only mode is explicitly partial and never reports COMPLETE PASS.")
    print("PASS: ordinary validation exits non-zero when R runtime validation cannot run.")
    return 0
if __name__=="__main__":sys.exit(main())
