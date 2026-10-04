# Otway_Rudkin_2024_Corrected31D — TDABM application source/evidence pack v1.0

This pack is an independently auditable application handover prepared to answer
`TDABM_APPLICATION_HANDOVER_REQUEST_v1_0.md`.

## Status
- Starting portable-framework stage: **P1.4-A**
- Authoritative analytical source: `source/merged_data.csv`
- Stable identifier: `ONSConstID`
- Human-readable label: `ConstituencyName`
- Final declared sample: **571**
- Topology dimensions: **31**
- Scaling: column-wise z-scores using the final-sample mean and sample SD
- Metric: Euclidean
- Current paper specification: **YES — corrected main specification**

## Critical instruction
Include `qualifications_level1` as the 31st topology variable. Use the corrected left-behind index defined in `registers/application_contract.csv` and `registers/colour_variable_register.csv`. The historical 30D application is provenance, not an automatic robustness check.

## Portability
Do not execute the absolute `setwd()` embedded in historical `dataprep.R`.
The receiving adapter must use paths relative to the portable project root and
must explicitly read `merged_data.csv` as CP1252/Windows-1252.

## Evidence hierarchy
1. `merged_data.csv` — authoritative analytical source.
2. `dataprep.R` — authoritative for historical preprocessing; historical reference for the corrected specification.
3. `ev1.dta` / `ev2.dta` — historical validation targets.
4. `ssrn-4877434.pdf` — source manuscript truth.
5. `bmc1.R`–`bmc4.R` — historical methodological reference only; not production framework code.
6. `registers/*.csv` — explicit handover contract and audit registers.

No current P1.4-A/P1.4-B/P1.4-C/P1.4-D analytical artifact exists in this pack.
