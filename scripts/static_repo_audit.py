#!/usr/bin/env python3
from pathlib import Path
import re, sys, subprocess

ROOT=Path(__file__).resolve().parents[1]
SELF=Path(__file__).resolve()
ons=re.compile(r"\b[EW]\d{8}\b")
forbidden={".dta",".sav",".rds",".rdata",".parquet",".feather",".xlsx",".xls",".gpkg",".shp",".dbf",".sqlite"}

# Avoid embedding the exact protected source list in the audit itself. These
# fragments are sufficient to detect accidental reintroduction of the historical
# hard-coded list into public source.
redwall_fragments=[
    "Bishop " + "Auckland",
    "Blyth " + "Valley",
    "Sedge" + "field",
    "Work" + "ington",
    "Bol" + "sover",
    "Rother " + "Valley",
    "Penistone and " + "Stocksbridge",
]

def tracked_files():
    if (ROOT/".git").exists():
        cp=subprocess.run(["git","ls-files","-z"],cwd=ROOT,capture_output=True)
        if cp.returncode==0:
            return [ROOT/p.decode() for p in cp.stdout.split(b"\0") if p]
    return [p for p in ROOT.rglob("*") if p.is_file()]

bad=[]
for p in tracked_files():
    if not p.is_file(): continue
    if p.resolve()==SELF: continue
    rel=p.relative_to(ROOT)
    if ".git" in rel.parts: continue
    if p.suffix.lower() in forbidden:
        bad.append((rel,"forbidden data extension")); continue
    if p.name=="merged_data.csv":
        bad.append((rel,"empirical merged_data.csv present")); continue
    if p.suffix.lower() in {".r",".py",".sh",".md",".txt",".csv",".yml",".yaml",".json",".cpp",".h",".hpp",".cff"}:
        try: txt=p.read_text(encoding="utf-8")
        except UnicodeDecodeError:
            try: txt=p.read_text(encoding="latin-1")
            except Exception: continue
        ids=set(ons.findall(txt))
        if len(ids)>=20:
            bad.append((rel,f"observation-level identifier payload ({len(ids)} IDs)"))
        if sum(x in txt for x in redwall_fragments)>=4:
            bad.append((rel,"embedded Red Wall source list"))
        if p.suffix.lower() in {".r",".py",".sh"} and "/media/simon/" in txt:
            bad.append((rel,"machine-specific executable path"))

if bad:
    print("FAIL: public repository audit")
    for rel,msg in bad: print(" -",rel,":",msg)
    sys.exit(1)
print("PASS: no prohibited empirical payloads or machine-specific executable paths")
