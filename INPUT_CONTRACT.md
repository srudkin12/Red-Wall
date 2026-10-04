# Input contract

## Core identifiers and electoral/context fields

The main input must provide the following non-topology fields used by the accepted application:

- `ONSConstID` — stable constituency identifier;
- `ConstituencyName` — constituency label;
- `Country`, `Region` — geographic strata/context;
- `Winner19` — 2019 winning party;
- `Con15`, `Con17`, `Con19` — Conservative vote shares;
- `Con1517`, `Con1719` — Conservative vote-share changes;
- `Lab15`, `Lab17`, `Lab19` — Labour vote shares;
- `Lab1517`, `Lab1719` — Labour vote-share changes;
- `leave` — constituency-level 2016 Leave estimate;
- `qualifications_below_a_level` — validation aggregate used to check the corrected education construction.

## 31 topology coordinates

| Variable | Reader-facing label | Domain | Raw units |
|---|---|---|---|
| `population_density` | Population density | population_density | residents_per_square_km |
| `age_0_15` | Age 0–15 | age | proportion_0_1 |
| `age_16_24` | Age 16–24 | age | proportion_0_1 |
| `age_25_34` | Age 25–34 | age | proportion_0_1 |
| `age_35_49` | Age 35–49 | age | proportion_0_1 |
| `age_50_64` | Age 50–64 | age | proportion_0_1 |
| `age_65_over` | Age 65 and over | age | proportion_0_1 |
| `ethnic_group_white_british` | White British | ethnicity | proportion_0_1 |
| `qualifications_none` | No qualifications | education | proportion_0_1 |
| `qualifications_level1` | Highest qualification: Level 1 | education | proportion_0_1 |
| `qualifications_level2` | Highest qualification: Level 2 | education | proportion_0_1 |
| `qualifications_level3` | Highest qualification: Level 3 | education | proportion_0_1 |
| `qualifications_level4` | Highest qualification: Level 4 | education | proportion_0_1 |
| `qualifications_other` | Highest qualification: Other | education | proportion_0_1 |
| `NSSEC_higher_managerial_admin_professional` | NS-SEC: Higher managerial, administrative and professional | NSSEC | proportion_0_1 |
| `NSSEC_lower_supervisory_technical` | NS-SEC: Lower supervisory and technical | NSSEC | proportion_0_1 |
| `NSSEC_semiroutine` | NS-SEC: Semi-routine | NSSEC | proportion_0_1 |
| `NSSEC_routine` | NS-SEC: Routine | NSSEC | proportion_0_1 |
| `NSSEC_never_worked_long_term_unemployed` | NS-SEC: Never worked or long-term unemployed | NSSEC | proportion_0_1 |
| `NSSEC_full_time_students` | NS-SEC: Full-time students | NSSEC | proportion_0_1 |
| `NSSEC_lower_managerial_admin_professional` | NS-SEC: Lower managerial, administrative and professional | NSSEC | proportion_0_1 |
| `NSSEC_intermediate_occupations` | NS-SEC: Intermediate occupations | NSSEC | proportion_0_1 |
| `NSSEC_small_employers_own_account_workers` | NS-SEC: Small employers and own-account workers | NSSEC | proportion_0_1 |
| `industry_agriculture_energy_water` | Industry: Agriculture, energy and water | industry | proportion_0_1 |
| `industry_manufacturing` | Industry: Manufacturing | industry | proportion_0_1 |
| `industry_construction` | Industry: Construction | industry | proportion_0_1 |
| `industry_distribution_hotels_restaurants` | Industry: Distribution, hotels and restaurants | industry | proportion_0_1 |
| `industry_transport_communication` | Industry: Transport and communication | industry | proportion_0_1 |
| `industry_financial_realestate_professional_administrative` | Industry: Financial, real estate, professional and administrative | industry | proportion_0_1 |
| `industry_public_administration_education_health` | Industry: Public administration, education and health | industry | proportion_0_1 |
| `industry_other` | Industry: Other | industry | proportion_0_1 |

Each topology coordinate is z-standardised after the final 571-seat sample is fixed. Euclidean distance defines local similarity.

## Sample contract

- candidate rows: 573;
- final analytical rows: 571;
- approved exclusions: Chorley (`E14000637`) and Buckingham (`E14000608`);
- `ONSConstID` must be unique and non-missing;
- topology variables must be complete in the final analytical sample.

## Historical exact-source marker

The recovered historical source file had SHA-256:

`79ac20539ee5dc2db7ffae8496996151bc5d7c6a2dd245bf1d8a3f9e958f5643`

A different byte-level source may be used only in documented reconstructed-source mode. The accepted canonical topology fingerprint remains the substantive replication gate.
