# Otway–Rudkin BJPolS Targeted Empirical Addendum v1.2

## Purpose

This package is a self-locating addendum to the existing Otway TDABM Portability tree.

It was written specifically for the established project layout:

`/path/to/otway-rudkin-red-wall-ball-mapper/`

No manual editing of R files is required.

The runner automatically discovers:

- the TDABM Portability repository root;
- `applications/Otway_Rudkin_2024_Corrected31D`;
- the accepted P1.4-A frozen input;
- the application contract and topology variables;
- the P1.4-C approved radius decision;
- the frozen P1.4-C canonical topology;
- the existing P1.4-B radius evidence;
- the Red Wall constituency definition from Kanagasooriam & Simon (2021), Figure 2 (42 published seats), matched automatically to ConstituencyName; Chorley is absent from the frozen 571-row sample, leaving the manuscript's 41 seats.

The script fails closed if it cannot reconcile the existing empirical objects.

## Analyses

1. **Canonical validation**
   - reconstructs Ball Mapper with the frozen 31D axes and canonical point order;
   - requires exact canonical headline structure: 28 balls, 96 edges, 2 components, 1 isolate;
   - where possible, also compares reconstructed memberships with the frozen membership file.

2. **Red Wall input-order robustness**
   - 1,000 random landmark orders at the approved Corrected31D radius;
   - records Red-Wall-specific diagnostics.

3. **Red Wall radius robustness**
   - automatically obtains a local set of radii from the prior P1.4-B/P1.4-C evidence;
   - falls back to a narrow ±0.10 band around the approved radius only if necessary;
   - repeats Red Wall diagnostics without reopening radius selection.

4. **Random-label concentration benchmark**
   - freezes the canonical topology;
   - compares the observed 41-seat Red Wall label with 10,000 random 41-seat labels.

5. **Conceptual-block weighting sensitivity**
   - preserves the canonical 31 z-scored axes;
   - weights density, age, ethnicity, qualification, industry and NSSEC blocks by `1/sqrt(k)`;
   - chooses a sensitivity radius using topology-only resolution matching;
   - then tests the Red Wall conclusion under that alternative metric weighting.

## Run

From the TDABM Portability root:

```bash
cd /path/to/otway-rudkin-red-wall-ball-mapper
rm -rf Otway_BJPolS_Targeted_Empirical_Addendum_v1_1
unzip -o Otway_BJPolS_Targeted_Empirical_Addendum_v1_1.zip \
  -d Otway_BJPolS_Targeted_Empirical_Addendum_v1_1

cd Otway_BJPolS_Targeted_Empirical_Addendum_v1_1
bash run_all.sh
```

No configuration file should be edited.

## Optional runtime controls

The defaults are designed for the established fast Linux workstation.

```bash
export OTWAY_ADDENDUM_WORKERS=20
export OTWAY_ORDER_REPS=1000
export OTWAY_RADIUS_REPS=1000
export OTWAY_RANDOM_LABEL_REPS=10000
export OTWAY_BLOCK_GRID_REPS=100
export OTWAY_BLOCK_FINAL_REPS=1000
```

Then run `bash run_all.sh`.

## Handback

A successful run creates a timestamped ZIP in:

`outputs/handback/`

Upload that ZIP back to ChatGPT.


## Red Wall source recovery

The earlier project handover explicitly recorded the historical local Red Wall lookup as unresolved.
v1.2 therefore does not search for or guess that missing historical object.

Instead, it uses the exact 42-seat definition published in:

Kanagasooriam, J. & Simon, E. (2021), *Red Wall: The Definitive Description*,
Political Insight 12(3):8–11, DOI 10.1177/20419058211045127.

The script normalises and matches those constituency names against the frozen P1.4-A input.
The accepted analytical sample excludes Chorley, so exactly 41 seats must match. The run fails
closed otherwise and writes a match audit.
