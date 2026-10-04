#!/usr/bin/env python3
from pathlib import Path
import argparse, shutil, sys

ROOT=Path(__file__).resolve().parents[1]

def fail(msg):
    print("FAIL:",msg,file=sys.stderr); raise SystemExit(1)

def cp(src,dst):
    src=ROOT/src; dst=ROOT/dst
    if not src.exists(): fail(f"Required generated artifact missing: {src}")
    dst.parent.mkdir(parents=True,exist_ok=True)
    shutil.copy2(src,dst)
    print(f"{src.relative_to(ROOT)} -> {dst.relative_to(ROOT)}")

def stage_2011_radius():
    base="Otway_BJPolS_2011_Census_Preflight_v1_6/outputs"
    tgt="Otway_BJPolS_2011_RoundedGrid_RadiusDiagnostics_v1_0"
    cp(f"{base}/10_2011_harmonised_candidate.csv",
       f"{tgt}/input/Otway_Rudkin_2011_Harmonised31D_input_freeze_candidate_v1_0.csv")
    cp(f"{base}/15_preflight_gates.csv",f"{tgt}/reference/v1_6_preflight_gates.csv")
    cp(f"{base}/PREFLIGHT_SUMMARY.md",f"{tgt}/reference/v1_6_PREFLIGHT_SUMMARY.md")
    cp(f"{base}/17_harmonisation_decision_record.csv",
       f"{tgt}/reference/v1_6_harmonisation_decision_record.csv")

def stage_2011_canonical():
    src="Otway_BJPolS_2011_RoundedGrid_RadiusDiagnostics_v1_0/outputs"
    tgt="Otway_BJPolS_2011_RoundedRadius_Canonical_RedWall_v1_1"
    cp(f"{src}/frozen_input/s2011_a_v1_0/Otway_Rudkin_2011_Harmonised31D_input_frozen_S2011_A_v1_0.csv",
       f"{tgt}/input/Otway_Rudkin_2011_Harmonised31D_input_frozen_S2011_A_v1_0.csv")
    cp(f"{src}/RADIUS_DIAGNOSTICS_SUMMARY.md",f"{tgt}/reference/2011_RADIUS_DIAGNOSTICS_SUMMARY.md")
    cp("Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1/outputs/04_canonical_extended_diagnostics.csv",
       f"{tgt}/reference/2021_canonical_extended_diagnostics.csv")

def stage_2011_serial():
    can="Otway_BJPolS_2011_RoundedRadius_Canonical_RedWall_v1_1/outputs"
    rob="Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1/outputs"
    tgt="Otway_BJPolS_2011_RedWall_Serial_Reproducibility_Audit_v1_0"
    cp("Otway_BJPolS_2011_RoundedGrid_RadiusDiagnostics_v1_0/outputs/frozen_input/s2011_a_v1_0/Otway_Rudkin_2011_Harmonised31D_input_frozen_S2011_A_v1_0.csv",
       f"{tgt}/input/Otway_Rudkin_2011_Harmonised31D_input_frozen_S2011_A_v1_0.csv")
    cp(f"{rob}/03_redwall_source_match_audit.csv",f"{tgt}/input/redwall_source_42_with_ons_ids.csv")
    cp(f"{can}/09_redwall_landmark_order_replicates.csv",f"{tgt}/reference/parallel_1000_replicates.csv")

def stage_2011_benchmarks():
    can="Otway_BJPolS_2011_RoundedRadius_Canonical_RedWall_v1_1/outputs"
    rob="Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1/outputs"
    tgt="Otway_BJPolS_2011_Matched_Concentration_Benchmarks_v1_0"
    mapping=[
      ("01_canonical_balls.csv","input/2011_canonical_balls.csv"),
      ("02_canonical_memberships.csv","input/2011_canonical_memberships.csv"),
      ("03_canonical_edges.csv","input/2011_canonical_edges.csv"),
      ("05_redwall_label_audit.csv","input/2011_redwall_label_audit.csv"),
      ("06_canonical_redwall_diagnostics.csv","reference/2011_canonical_redwall_diagnostics.csv"),
    ]
    for s,d in mapping: cp(f"{can}/{s}",f"{tgt}/{d}")
    cp(f"{rob}/14b_electoral_history_proxy_audit.csv",f"{tgt}/input/2021_electoral_history_proxy_audit.csv")
    cp(f"{rob}/15_benchmark_replicates.csv",f"{tgt}/reference/2021_benchmark_replicates.csv")
    cp(f"{rob}/16_benchmark_summary.csv",f"{tgt}/reference/2021_benchmark_summary.csv")
    cp(f"{can}/HISTORICAL_SENSITIVITY_SUMMARY.md",f"{tgt}/reference/2011_HISTORICAL_SENSITIVITY_SUMMARY.md")

def stage_temporal():
    bench="Otway_BJPolS_2011_Matched_Concentration_Benchmarks_v1_0/outputs"
    can="Otway_BJPolS_2011_RoundedRadius_Canonical_RedWall_v1_1/outputs"
    rob="Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1/outputs"
    tgt="Otway_BJPolS_2011_2021_Temporal_Concentration_Contrast_v1_0/input"
    cp(f"{bench}/01_2011_benchmark_replicates.csv",f"{tgt}/2011_benchmark_replicates.csv")
    cp(f"{rob}/15_benchmark_replicates.csv",f"{tgt}/2021_benchmark_replicates.csv")
    cp(f"{can}/06_canonical_redwall_diagnostics.csv",f"{tgt}/2011_canonical_redwall_diagnostics.csv")
    cp(f"{rob}/04_canonical_extended_diagnostics.csv",f"{tgt}/2021_canonical_extended_diagnostics.csv")
    cp(f"{bench}/03_paired_draw_contract.csv",f"{tgt}/paired_draw_contract.csv")

def stage_v16():
    can="Otway_BJPolS_2011_RoundedRadius_Canonical_RedWall_v1_1/outputs"
    bench="Otway_BJPolS_2011_Matched_Concentration_Benchmarks_v1_0/outputs"
    rob="Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1/outputs"
    targ="Otway_BJPolS_Targeted_Empirical_Addendum_v1_2/outputs"
    topo="applications/Otway_Rudkin_2024_Corrected31D/frozen_topology/p1_4_c_v1_0"
    tgt="Otway_BJPolS_v1_6_Adversarial_Response_Evidence_v1_0/input"
    pairs=[
      (f"{rob}/04_canonical_extended_diagnostics.csv","2021_canonical_extended_diagnostics.csv"),
      (f"{rob}/15_benchmark_replicates.csv","2021_benchmark_replicates.csv"),
      (f"{can}/06_canonical_redwall_diagnostics.csv","2011_canonical_redwall_diagnostics.csv"),
      (f"{bench}/01_2011_benchmark_replicates.csv","2011_benchmark_replicates.csv"),
      (f"{topo}/canonical_scaled_axes.csv","2021_canonical_scaled_axes.csv"),
      (f"{rob}/03_redwall_source_match_audit.csv","2021_redwall_source_match_audit.csv"),
      (f"{topo}/canonical_memberships.csv","2021_canonical_memberships.csv"),
      (f"{rob}/17_canonical_ball_diagnostics.csv","2021_canonical_ball_diagnostics.csv"),
      (f"{topo}/canonical_ball_components.csv","2021_canonical_ball_components.csv"),
      (f"{rob}/18_canonical_edge_diagnostics.csv","2021_canonical_edge_diagnostics.csv"),
      (f"{targ}/11_block_weighting_redwall_summary.csv","accepted_block_weighting_redwall_summary.csv"),
      (f"{targ}/12_block_weighting_definition.csv","accepted_block_weighting_definition.csv"),
      (f"{targ}/ACCEPTANCE.txt","accepted_block_weighting_ACCEPTANCE.txt"),
    ]
    for s,d in pairs: cp(s,f"{tgt}/{d}")

def stage_figures():
    topo="applications/Otway_Rudkin_2024_Corrected31D/frozen_topology/p1_4_c_v1_0"
    rob="Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1/outputs"
    ref="figure_production/Otway_BJPolS_Final_Figure_Production_v1_1/reference"
    for name in ("canonical_memberships.csv","canonical_overlap_edges.csv","canonical_ball_components.csv",
                 "canonical_topology_summary.csv","canonical_topology_fingerprint.sha256"):
        cp(f"{topo}/{name}",f"{ref}/{name}")
    cp(f"{rob}/03_redwall_source_match_audit.csv",f"{ref}/redwall_source_match_audit.csv")

STAGES={
 "2011-radius":stage_2011_radius,
 "2011-canonical":stage_2011_canonical,
 "2011-serial":stage_2011_serial,
 "2011-benchmarks":stage_2011_benchmarks,
 "temporal":stage_temporal,
 "v1-6":stage_v16,
 "figures":stage_figures,
}

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument("stage",choices=list(STAGES)+["all"])
    a=ap.parse_args()
    if a.stage=="all":
        for name,fn in STAGES.items():
            print(f"\n== {name} =="); fn()
    else:
        STAGES[a.stage]()
    print("PASS:",a.stage)

if __name__=="__main__": main()
