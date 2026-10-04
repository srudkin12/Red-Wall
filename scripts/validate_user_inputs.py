#!/usr/bin/env python3
from pathlib import Path
import csv, hashlib, math, sys

ROOT=Path(__file__).resolve().parents[1]
merged=ROOT/"data"/"user"/"merged_data.csv"
rw=ROOT/"data"/"user"/"redwall_source.csv"
contract=ROOT/"applications"/"Otway_Rudkin_2024_Corrected31D"/"config"/"application_contract.csv"
EXPECTED_SHA="79ac20539ee5dc2db7ffae8496996151bc5d7c6a2dd245bf1d8a3f9e958f5643"

def fail(x):
    print("FAIL:",x,file=sys.stderr); raise SystemExit(1)

if not merged.exists(): fail(f"Missing {merged}; see DATA_SOURCING.md")
if not rw.exists(): fail(f"Missing {rw}; see DATA_SOURCING.md")

fields={}
with contract.open(encoding="utf-8-sig") as f:
    for r in csv.DictReader(f): fields[r["field"]]=r["value"]

topology=[x for x in fields["topology_variables"].split(";") if x]
required={
    fields["id_col"],fields["label_col"],"Country","Region","Winner19",
    "Con15","Con17","Con19","Con1517","Con1719",
    "Lab15","Lab17","Lab19","Lab1517","Lab1719","leave",
    "qualifications_below_a_level"
}
required.update(topology)

text=merged.read_text(encoding="cp1252")
rows=list(csv.DictReader(text.splitlines()))
if len(rows)!=573: fail(f"Expected 573 candidate rows; found {len(rows)}")
if len(rows[0])!=84: fail(f"Expected 84 source columns; found {len(rows[0])}")
missing=sorted(required-set(rows[0]))
if missing: fail("Missing required columns: "+", ".join(missing))

id_col=fields["id_col"]; label_col=fields["label_col"]
ids=[r[id_col].strip() for r in rows]
if any(not x for x in ids): fail("ONSConstID contains blank values")
if len(set(ids))!=573: fail("ONSConstID must be unique across 573 candidate rows")

expected_exclusions={"E14000637":"Chorley","E14000608":"Buckingham"}
by_id={r[id_col].strip():r for r in rows}
for oid,name in expected_exclusions.items():
    if oid not in by_id: fail(f"Missing approved exclusion {oid} {name}")
    if by_id[oid][label_col].strip()!=name:
        fail(f"Approved exclusion label mismatch for {oid}: {by_id[oid][label_col]!r}")

final=[r for r in rows if r[id_col].strip() not in expected_exclusions]
if len(final)!=571: fail(f"Expected 571 final rows; found {len(final)}")

for var in topology:
    vals=[]
    for r in final:
        raw=r[var].strip()
        if raw=="" or raw.upper()=="NA": fail(f"Missing topology value in {var}")
        try: x=float(raw)
        except ValueError: fail(f"Non-numeric topology value in {var}: {raw!r}")
        if not math.isfinite(x): fail(f"Non-finite topology value in {var}")
        vals.append(x)
    if max(vals)==min(vals): fail(f"Topology variable is constant: {var}")

with rw.open(encoding="utf-8-sig") as f:
    rr=list(csv.DictReader(f))
if len(rr)!=42: fail(f"redwall_source.csv must contain 42 rows; found {len(rr)}")
for c in ("source_constituency","source_region"):
    if c not in rr[0]: fail(f"redwall_source.csv missing {c}")
names=[r["source_constituency"].strip() for r in rr]
if any(not x for x in names): fail("Red Wall source contains blank constituency names")
if len(set(names))!=42: fail("Red Wall source constituency names must be unique")

h=hashlib.sha256(merged.read_bytes()).hexdigest()
print("PASS: user input schema")
print("candidate_rows=573")
print("source_columns=84")
print("final_rows=571")
print("topology_coordinates=",len(topology),sep="")
print("sha256=",h,sep="")
print("historical_exact_file_match=",h==EXPECTED_SHA,sep="")
if h!=EXPECTED_SHA:
    print("NOTE: byte-level source differs; prepare_local_inputs.sh will use reconstructed-source mode.")
