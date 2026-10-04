# Otway_Rudkin_2024_Corrected31D
## TDABM Application Handover Response v1.0

**Prepared:** 2026-08-10  
**Governing request:** `TDABM_APPLICATION_HANDOVER_REQUEST_v1_0.md`  
**Recommended starting stage:** **P1.4-A**

# Executive summary

**YES.** This application can be reconstructed as an explicit, independently auditable TDABM application contract from the supplied evidence, without relying on another conversation's memory.

The authoritative analytical source is `merged_data.csv` (573 x 84; SHA-256 `79ac20539ee5dc2db7ffae8496996151bc5d7c6a2dd245bf1d8a3f9e958f5643`). The stable unit identifier is `ONSConstID`. The final declared analysis population is 571 England/Wales constituencies after excluding Chorley (`E14000637`) and Buckingham (`E14000608`) for the Speaker/former-Speaker comparability reason documented in the manuscript.

**Application role:** current corrected main paper specification; not robustness.

No current portable-framework analytical stage has yet been accepted. Begin at P1.4-A.

# 1. Application identity

- **Application ID:** `Otway_Rudkin_2024_Corrected31D`
- **Paper/project:** *The Shape of Left-Behindedness: The 2019 UK General Election and Britain’s Changing Electoral Geography*
- **TDABM purpose:** represent constituency demographic similarity in a high-dimensional Census 2021 characteristic space and colour the fixed topology by electoral/context variables.
- **Role:** corrected main paper specification
- **Relevant manuscript material:** §§3.1–3.3; Figures 4–15; robustness §5.
- **Historical labels:** `ev1.dta`, `ev2.dta`, `dataprep.R`; legacy `bmc1.R`–`bmc4.R`.
- **Relationship to the other application:** Supersedes Historical30D for current-paper analysis because Level 1 should have been included.

# 2. Authoritative source data

The full inventory, SHA-256 hashes, roles and inclusion status are in `registers/source_provenance.csv`.

Source hierarchy:
1. `merged_data.csv`: authoritative rectangular analytical source.
2. `dataprep.R`: authoritative historical reconstruction code; contains an obsolete machine-specific `setwd()` and must not be executed unchanged.
3. `ev1.dta`, `ev2.dta`: historical validation targets.
4. `ssrn-4877434.pdf`: manuscript/source truth for study purpose, sample rationale, variable descriptions and historical outputs.
5. `bmc1.R`–`bmc4.R`: historical method/reference code only.

Upstream raw Census/BES/election assembly files are not present. This is recorded as non-blocking for P1.4-A because `merged_data.csv` is sufficient to reconstruct the application-level analytical input.

# 3. Unit identifier and labels

- **Stable ID:** `ONSConstID`; 573/573 unique in source.
- **Reader label:** `ConstituencyName`.
- **Historical/order field:** `pano` is retained as provenance/context only.
- **Canonical point order:** receiving portable framework should sort deterministically by stable ID. Historical CSV order is not analytically privileged.
- The source CSV is CP1252/Windows-1252. Do not assume UTF-8.

# 4. Exact topology-variable contract

- **Topology dimensions:** **31**
- Complete row-level definitions, raw observed ranges, labels and transformations are in `registers/variable_register.csv`.

The corrected specification contains all historical 30 topology variables **plus** `qualifications_level1`, yielding 31 standardized coordinates. This is the current paper specification by explicit project decision. It is a correction of an omission, not a robustness branch.

Only the variables in that register are topology coordinates. All other source columns are explicitly classified in `registers/source_column_role_register.csv`. Electoral outcomes, Leave vote, IDs/labels/geography and derived/context fields must not enter radius diagnostics.

# 5. Scaling and metric

- **Scaling:** after the final N=571 sample is fixed, standardize each declared topology coordinate as `(x - mean(x)) / sd(x)` using the sample standard deviation (`n-1`), matching R `scale()` semantics.
- **Metric:** Euclidean.
- **Explicit axis weights:** none.
- **Preprocessing:** no PCA, no compositional reduction, no outcome-dependent transformation.
- **Verified historical equivalence:** reconstructing historical30D raw topology from `merged_data.csv` matches `ev1.dta` to max absolute difference `9.71e-17`; z-scoring matches `ev2.dta` to `9.21e-15`.

The centered corrected 31-coordinate matrix has verified rank 27. Age, education, NS-SEC and industry are complete compositional blocks. Adding Level 1 therefore changes Euclidean geometry without increasing linear rank. Preserve all 31 declared coordinates; do not silently collapse or reweight them.

# 6. Sample construction and missing-data rule

Detailed independent sample audit: `registers/sample_audit.csv`.

- Candidate rows: 573.
- Stable IDs: 573 unique; no duplicates.
- Missing topology cells: 0 in both 30D and corrected31D candidate matrices.
- Excluded units: Chorley (`E14000637`) and Buckingham (`E14000608`).
- Final N: 571.

Corrected31D declares the two Speaker seats as explicit substantive analysis-population exclusions using stable IDs. It does not inherit a complete-case rule from Historical30D.

# 7. Colouring/context variables

See `registers/colour_variable_register.csv`.

Verified core colourings include Conservative and Labour vote shares for 2015/2017/2019, corresponding source change fields, and the 2016 Leave estimate. These variables are post-topology colour/context variables and cannot select radius.

Corrected conventional left-behind index:
1. `density_inverse_01 = 1 - (pd - min(pd)) / (max(pd) - min(pd))`, computed on the final N=571 sample.
2. `qualifications_below_level3 = qualifications_none + qualifications_level1 + qualifications_level2`.
3. `lbhi_raw_corrected = rowMeans(density_inverse_01, white_british, qualifications_below_level3, routine, manufacturing)`.
4. `left_behind_index_corrected = minmax(lbhi_raw_corrected)`.
The source field `qualifications_below_a_level` independently matches the explicit education sum to max absolute difference {qbelow_diff:.3g}; the explicit component formula remains the contract.

Three historical paper elements remain non-blocking but unresolved: exact two-party swing formula for Figure 6(l), the constituency Red Wall lookup for Figure 15, and exact historical K-means initialization/assignments for Figures 11–12.

# 8. Historical Ball Mapper/radius information

See `registers/historical_radius_register.csv` and `registers/historical_topology_summary.csv`.

No historical corrected31D topology or radius exists. This is expected: Corrected31D is a new specification. P1.4-B must therefore run fresh topology-only diagnostics and an explicit user radius decision must precede P1.4-C.

Any old radius information is `HISTORICAL_REFERENCE_ONLY`; it cannot approve a current portable radius.

# 9. Existing code

- `dataprep.R`: **authoritative historical reconstruction code** for the old 30D sample/variables/scaling and historical index; **historical reference** for corrected31D.
- `bmc1.R`–`bmc4.R`: **HISTORICAL_REFERENCE_ONLY**. They are not the current framework and do not establish the main plot radius.
- No supplied code is approved to replace generic P1.4-A/B/C/D logic.

# 10. Existing accepted analytical artifacts

`registers/accepted_artifact_register.csv` records that no current portable-framework analytical artifact exists. Start at **P1.4-A**.

# 11. Model-diagnostic requirements

`NO VERIFIED HISTORICAL MODEL SPECIFICATION FOUND`.

The paper's K-means exercise is a clustering robustness comparator, not a verified fitted-model/residual diagnostic contract. See `registers/model_diagnostic_register.csv`.

# 12. Publication-facing labels

Reader-facing topology labels are in `registers/variable_register.csv`; paper-facing colour labels are in `registers/colour_variable_register.csv`.

Historical terminology retained for provenance includes:
- “Conventional Left-Behind Index”
- “Conservative vote 2019”
- “Labour vote 2019”
- “Conservative vote change (2015–2017)”
- “Conservative vote change (2017–2019)”
- “Leave vote (2016 EU Referendum)”

Internal source variable names remain authoritative in the analytical contract.

# 13. Multiple specifications and firewall

Treat `Otway_Rudkin_2024_Historical30D` and `Otway_Rudkin_2024_Corrected31D` as independent applications.

Do not silently share:
- topology-variable count;
- radius diagnostics;
- approved radius;
- canonical topology;
- fingerprints or memberships.

The N=571 sample is independently justified in each application.

**Project decision:** Corrected31D is the current paper specification. Historical30D is provenance/reproduction and is **not** to be presented as a robustness check merely because it exists.

# 14. Machine-readable registers supplied

- `application_contract.csv`
- `source_provenance.csv`
- `variable_register.csv`
- `sample_audit.csv`
- `colour_variable_register.csv`
- `historical_radius_register.csv`
- `historical_topology_summary.csv`
- `model_diagnostic_register.csv`
- `accepted_artifact_register.csv`
- `unresolved_items.csv`
- `source_column_role_register.csv` (additional audit)
- `source_validation_summary.csv` (additional audit)
- `left_behind_index_validation.csv` (additional audit)
- `SHA256SUMS.txt`

# 15. Validation targets and warnings

Verified numerical targets are in `evidence/source_validation_summary.csv`.

Notable checks:
- 573 source rows; 84 columns.
- 573 unique `ONSConstID`.
- 0 missing topology cells in candidate 30D and 31D matrices.
- historical final N=571.
- historical `ev1.dta` and `ev2.dta` reconstruction within machine precision.
- `qualifications_below_a_level = none + level1 + level2` within floating-point precision.
- corrected left-behind index is intentionally not identical to the historical implementation.
- source `Con1719` and `Lab1719` for Milton Keynes South differ slightly from direct share subtraction; preserve source values unless a separate project decision authorizes recomputation.

# 16. Unresolved items

See `registers/unresolved_items.csv`. No listed item blocks P1.4-A.

# 17. Required portable-framework sequence

```text
this authoritative application handover
            ↓
P1.4-A source reconstruction + immutable input freeze
            ↓
P1.4-B topology-only radius diagnostics
            ↓
explicit user radius decision
            ↓
P1.4-C canonical fixed-topology freeze
            ↓
post-freeze recolouring
            ↓
P1.4-D paper evidence + interpretation robustness
```

Outcome/colour variables must not select radius; diagnostic radius references remain non-binding; automatic radius selection and optimality claims are prohibited.

# 18. Handover conclusion

**PASS FOR ENTRY TO P1.4-A.**

The supplied evidence is sufficient to reconstruct this application contract independently. The corrected31D application should become the primary Otway–Rudkin paper analysis once its P1.4-A freeze, P1.4-B diagnostics, explicit radius decision and P1.4-C topology freeze have passed.
