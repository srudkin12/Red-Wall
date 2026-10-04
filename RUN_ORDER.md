# Reproduction order

Run all commands from the repository root. Keep the shell in which
`prepare_local_inputs.sh` was sourced so that the data-source mode and Red Wall
path remain available.

## A. Input validation and dependencies

```bash
python3 scripts/validate_user_inputs.py
source scripts/prepare_local_inputs.sh
Rscript tools/install_dependencies.R
```

## B. 2021 Corrected31D input and canonical topology

Build the deterministic analytical input:

```bash
bash applications/Otway_Rudkin_2024_Corrected31D/scripts/run_p1_4_a.sh
```

Build the paper's fixed canonical topology at the approved radius 5.50:

```bash
bash scripts/run_public_canonical.sh
```

The canonical builder requires the accepted topology fingerprint by default.

### Optional: reproduce the full topology-only radius screen

The production radius screen is computationally expensive (386 radii × 1,000
landmark orders). It is retained for complete methodological reproduction:

```bash
bash applications/Otway_Rudkin_2024_Corrected31D/scripts/run_p1_4_b.sh production
```

The historical P1.4-C script is retained as provenance for the original
decision chain. Public reconstruction uses `run_public_canonical.sh` because
the original P1.4-C script deliberately requires the historical B-audit ZIP
hash.

## C. 2021 robustness and calibration

```bash
bash Otway_BJPolS_Targeted_Empirical_Addendum_v1_2/run_all.sh
bash Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1/run_all.sh
```

## D. Census 2011 harmonisation

```bash
bash Otway_BJPolS_2011_Census_Preflight_v1_6/run_all.sh
python3 scripts/stage_generated_inputs.py 2011-radius

bash Otway_BJPolS_2011_RoundedGrid_RadiusDiagnostics_v1_0/run_all.sh
python3 scripts/stage_generated_inputs.py 2011-canonical

bash Otway_BJPolS_2011_RoundedRadius_Canonical_RedWall_v1_1/run_all.sh
```

## E. 2011 reproducibility audit (optional verification)

This audit independently recomputes the 1,000 landmark-order labelled runs on
one worker. It is a provenance verification rather than a distinct manuscript
estimand.

```bash
python3 scripts/stage_generated_inputs.py 2011-serial
bash Otway_BJPolS_2011_RedWall_Serial_Reproducibility_Audit_v1_0/run_all.sh
```

## F. Matched 2011 benchmarks and 2011→2021 temporal contrast

```bash
python3 scripts/stage_generated_inputs.py 2011-benchmarks
bash Otway_BJPolS_2011_Matched_Concentration_Benchmarks_v1_0/run_all.sh

python3 scripts/stage_generated_inputs.py temporal
bash Otway_BJPolS_2011_2021_Temporal_Concentration_Contrast_v1_0/run_all.sh
```

## G. v1.6 prevalence/mixing and neighbourhood evidence

```bash
python3 scripts/stage_generated_inputs.py v1-6
bash Otway_BJPolS_v1_6_Adversarial_Response_Evidence_v1_0/run_all.sh
```

## H. Final Figures 3 and 4

```bash
python3 scripts/prepare_figure_inputs.py
bash figure_production/Otway_BJPolS_Final_Figure_Production_v1_1/run_all.sh
```

The public figure runner regenerates Figures 3 and 4. The manuscript-DOCX
patching script is retained for provenance but is not executed by the public
runner because the submission manuscript is not distributed with this code
repository.

## I. Repository safety check

Before committing:

```bash
python3 scripts/static_repo_audit.py
```

After `git init`, the audit examines tracked files, so locally generated ignored
data do not cause false failures.
