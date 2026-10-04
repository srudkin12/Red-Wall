#!/usr/bin/env python3
from pathlib import Path
import argparse,csv,hashlib,sys
def sha(p):
    h=hashlib.sha256()
    with p.open("rb") as f:
        for b in iter(lambda:f.read(1024*1024),b""): h.update(b)
    return h.hexdigest()
def main():
    ap=argparse.ArgumentParser();ap.add_argument("root",nargs="?");args=ap.parse_args()
    root=Path(args.root).resolve() if args.root else Path(__file__).resolve().parents[1]
    mf=root/"manifests/CLEAN_BASELINE_MANIFEST.csv"
    if not mf.is_file():
        print("FAIL: missing clean baseline manifest");return 1
    with mf.open(newline="",encoding="utf-8") as f: rows=list(csv.DictReader(f))
    reg={r["relative_path"]:r for r in rows};problems=[];actual=[]
    for p in sorted(root.rglob("*")):
        if p.is_file() and p!=mf:
            rel=p.relative_to(root).as_posix();actual.append(rel)
            r=reg.get(rel)
            if r is None: problems.append(f"unregistered: {rel}");continue
            if int(r["byte_size"])!=p.stat().st_size: problems.append(f"size mismatch: {rel}")
            if r["sha256"]!=sha(p): problems.append(f"sha mismatch: {rel}")
    for rel in reg:
        if rel not in actual: problems.append(f"registered missing: {rel}")
    print("TDABM clean baseline manifest check")
    print(f"Registered files: {len(rows)}")
    print(f"Problems: {len(problems)}")
    for x in problems[:40]:print("FAIL:",x)
    if not problems:print("PASS: clean baseline file set, byte sizes and SHA-256 hashes are exact.")
    return 1 if problems else 0
if __name__=="__main__":sys.exit(main())
