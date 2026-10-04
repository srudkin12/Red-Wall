#!/usr/bin/env python3
from pathlib import Path
import shutil

ROOT=Path(__file__).resolve().parents[1]
APP=ROOT/"applications"/"Otway_Rudkin_2024_Corrected31D"
FIG=ROOT/"figure_production"/"Otway_BJPolS_Final_Figure_Production_v1_1"
REF=FIG/"reference"
REF.mkdir(parents=True,exist_ok=True)

topo=APP/"frozen_topology"/"p1_4_c_v1_0"
rob=ROOT/"Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1"/"outputs"

for name in (
    "canonical_memberships.csv",
    "canonical_overlap_edges.csv",
    "canonical_ball_components.csv",
    "canonical_topology_summary.csv",
    "canonical_topology_fingerprint.sha256",
):
    src=topo/name
    if not src.exists():
        raise SystemExit(f"Missing locally generated canonical artifact: {src}")
    shutil.copy2(src,REF/name)

rw=rob/"03_redwall_source_match_audit.csv"
if not rw.exists():
    raise SystemExit("Missing 2021 Red Wall match audit. Run rounded-radius robustness first.")
shutil.copy2(rw,REF/"redwall_source_match_audit.csv")
print("PASS: final-figure empirical reference inputs prepared from local generated outputs")
