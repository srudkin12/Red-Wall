#!/usr/bin/env python3
from pathlib import Path
import argparse,hashlib,re,sys
def main():
    ap=argparse.ArgumentParser();ap.add_argument("root");ap.add_argument("archive");ap.add_argument("sidecar");a=ap.parse_args()
    root=Path(a.root);arc=Path(a.archive);side=Path(a.sidecar);vals={}
    for l in (root/"VERSION").read_text().splitlines():
        if "=" in l:
            k,v=l.split("=",1);vals[k.strip()]=v.strip()
    v=vals.get("package_version","");expected="TDABM_Portability_Clean_Start_v"+v.replace(".","_")+".zip";pro=[]
    if not re.fullmatch(r"\d+\.\d+\.\d+",v):pro.append("invalid VERSION package_version")
    if arc.name!=expected:pro.append("archive basename mismatch")
    if side.name!=expected+".sha256":pro.append("sidecar basename mismatch")
    if arc.is_file() and side.is_file():
        h=hashlib.sha256(arc.read_bytes()).hexdigest();parts=side.read_text().strip().split()
        if len(parts)!=2 or parts[0].lower()!=h or parts[1]!=arc.name:pro.append("sidecar content mismatch")
    else:pro.append("archive or sidecar missing")
    print("TDABM clean release-identity check");print("VERSION package_version:",v);print("Expected archive:",expected);print("Problems:",len(pro))
    for x in pro:print("FAIL:",x)
    if not pro:print("PASS: VERSION, archive basename, sidecar basename and sidecar line are consistent.")
    return 1 if pro else 0
if __name__=="__main__":sys.exit(main())
