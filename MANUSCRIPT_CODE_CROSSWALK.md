# Manuscript-to-code crosswalk

This crosswalk identifies the principal public code responsible for the manuscript's empirical claims. Generated data and outputs are deliberately not committed.

| Manuscript element | Principal code |
|---|---|
| 2021 571-seat Corrected31D analytical input | `applications/Otway_Rudkin_2024_Corrected31D/scripts/01_build_p1_4_a.R` |
| 2021 topology-only radius diagnostics | `applications/Otway_Rudkin_2024_Corrected31D/scripts/02_run_p1_4_b_radius_diagnostics.R` |
| Canonical radius 5.50 topology | `scripts/build_public_canonical.R` using the accepted P1.4-C framework/helpers |
| Red Wall order/radius, random-label and domain-weighting checks | `Otway_BJPolS_Targeted_Empirical_Addendum_v1_2/run_targeted_addendum_v1_2.R` |
| Rounded-radius ILR/LOBO and staged 2021 benchmarks | `Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1/run_2021_rounded_radius_robustness_v1_1.R` |
| Census 2011 acquisition and harmonisation | `Otway_BJPolS_2011_Census_Preflight_v1_6/run_2011_census_preflight_v1_6.R` |
| 2011 rounded-grid radius diagnostics | `Otway_BJPolS_2011_RoundedGrid_RadiusDiagnostics_v1_0/run_2011_rounded_grid_radius_diagnostics_v1_0.R` |
| 2011 canonical Red Wall topology at radius 5.75 | `Otway_BJPolS_2011_RoundedRadius_Canonical_RedWall_v1_1/run_2011_rounded_radius_canonical_redwall_v1_1.R` |
| Independent single-core 2011 reproducibility audit | `Otway_BJPolS_2011_RedWall_Serial_Reproducibility_Audit_v1_0/run_serial_reproducibility_audit_v1_0.R` |
| Matched 2011 concentration benchmarks | `Otway_BJPolS_2011_Matched_Concentration_Benchmarks_v1_0/run_2011_matched_concentration_benchmarks_v1_0.R` |
| Paired 2011→2021 concentration change | `Otway_BJPolS_2011_2021_Temporal_Concentration_Contrast_v1_0/run_temporal_concentration_contrast_v1_0.R` |
| Size-aware prevalence/mixing calibration and high-prevalence neighbourhood profiles | `Otway_BJPolS_v1_6_Adversarial_Response_Evidence_v1_0/scripts/run_v1_6_evidence.py` |
| Final Figures 3 and 4 | `figure_production/Otway_BJPolS_Final_Figure_Production_v1_1/scripts/generate_final_figures.py` |

## Figures 1 and 2

The recovered project archive does not contain a separate authoritative production script for manuscript Figure 1 or the original Figure 2 rendering. Figure 2's accepted fixed node layout is retained because it is required by the authoritative Figure 3/4 production source. The canonical topology and party colourings underlying Figure 2 are reproducible from the public analytical pipeline.

The repository does not invent missing historical plotting provenance.
