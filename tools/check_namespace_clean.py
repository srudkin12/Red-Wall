#!/usr/bin/env python3
from pathlib import Path
import argparse, re, sys

# Conservative framework-only check for calls that caused prior clean-session
# failures. It intentionally does not attempt to be a complete R linter.
FORBIDDEN = [
    re.compile(r"(?<![:A-Za-z0-9_.])uniqueN\s*\("),
    re.compile(r"(?<![:A-Za-z0-9_.])fifelse\s*\("),
]
def strip_comments(text):
    out=[]
    for line in text.splitlines():
        quote=None; esc=False; cut=len(line)
        for i,ch in enumerate(line):
            if esc: esc=False; continue
            if ch=="\\" and quote: esc=True; continue
            if quote:
                if ch==quote: quote=None
                continue
            if ch in ("'",'"'): quote=ch; continue
            if ch=="#": cut=i; break
        out.append(line[:cut])
    return "\n".join(out)

def main():
    ap=argparse.ArgumentParser();ap.add_argument("root", nargs="?");args=ap.parse_args()
    root=Path(args.root).resolve() if args.root else Path(__file__).resolve().parents[1]
    failures=[]
    for p in (root/"framework").rglob("*.R"):
        text=strip_comments(p.read_text(encoding="utf-8",errors="replace"))
        for pat in FORBIDDEN:
            for m in pat.finditer(text):
                line=text.count("\n",0,m.start())+1
                failures.append(f"{p.relative_to(root)}:{line}: unnamespaced governed call")
    print("TDABM clean namespace check")
    print(f"Violations: {len(failures)}")
    for x in failures: print("FAIL:",x)
    if not failures:
        print("PASS: governed data.table helper calls are explicitly namespaced in framework source.")
    return 1 if failures else 0

if __name__=="__main__": sys.exit(main())
