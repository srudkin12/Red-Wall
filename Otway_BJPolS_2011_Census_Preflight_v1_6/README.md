# Otway–Rudkin BJPolS: 2011 Census preflight / harmonisation v1.6

## Purpose

This package performs a **preflight only** for a harmonised 2011 Census sensitivity analysis supporting the Otway–Rudkin BJPolS manuscript.

It does **not** construct or select a 2011 Ball Mapper topology. Its job is to determine whether a defensible 2011 analogue of the frozen Corrected31D socio-demographic frame can be constructed before any topology is fitted.

The package:

1. discovers the authoritative `TDABM_Portability` repository and frozen Corrected31D input;
2. inventories the 31 frozen 2021 socio-demographic coordinates and their block behaviour;
3. downloads official 2011 Census parliamentary-constituency data from the Nomis API for:
   - QS102EW population density;
   - QS103EW single-year age;
   - KS201EW ethnic group;
   - KS501EW qualifications;
   - KS605EW industry;
   - KS611EW NS-SEC;
4. queries the frozen 571 England-and-Wales constituencies directly by their authoritative GSS codes;
5. requires every one of the six Nomis tables to return exactly that frozen 571-code frame;
6. attempts an explicit 31-coordinate 2011 harmonisation, with no silent category substitution;
7. audits every 2011 category used in each target coordinate;
8. reports conceptual AMBER issues separately from computational failures;
9. writes a manuscript-facing preflight summary and a full provenance manifest;
10. always attempts to create a timestamped handback ZIP, even if the R stage fails.

## Important methodological rule

A successful preflight does **not** approve a 2011 Ball Mapper analysis automatically. It only establishes whether the 2011 data frame can be built with sufficiently transparent correspondence to the frozen 2021 frame.

The following are deliberately treated cautiously:

- the six-coordinate qualifications block omits the separate apprenticeship category, so the code audits whether the frozen 2021 block behaves consistently with that omission rather than silently reallocating apprenticeships;
- the 2011 KS611EW NS-SEC table covers usual residents aged 16–74; the code flags any unresolved denominator comparability with the frozen 2021 construction;
- population-density units can differ by a constant factor without affecting within-census z-standardisation, but the source unit is preserved in the audit;
- no 2011 radius is borrowed from the 2021 analysis.

## Network access

The run requires internet access to `https://www.nomisweb.co.uk/`.

Nomis permits anonymous API use but limits anonymous requests to 25,000 cells. This package keeps individual requests well below that limit by downloading constituencies in batches.

If you already have a Nomis Unique ID, you may optionally set it before running:

```bash
export NOMIS_UID='0x...'
```

The UID is treated as a secret: it is never written to logs or output URLs. A UID is **not required** for the normal batched run.

## Run

Place this ZIP in:

```text
/path/to/otway-rudkin-red-wall-ball-mapper/
```

Then:

```bash
cd /path/to/otway-rudkin-red-wall-ball-mapper
unzip -o Otway_BJPolS_2011_Census_Preflight_v1_6.zip
cd Otway_BJPolS_2011_Census_Preflight_v1_6
chmod +x run_all.sh
./run_all.sh
```

Optional smaller geography batches can be requested if the Nomis service is unstable:

```bash
OTWAY_NOMIS_BATCH=15 ./run_all.sh
```

## Handback

At the end, upload both files created under `handback/`:

```text
Otway_BJPolS_2011_Census_Preflight_v1_6_Handback_YYYYMMDD_HHMMSS.zip
Otway_BJPolS_2011_Census_Preflight_v1_6_Handback_YYYYMMDD_HHMMSS.zip.sha256
```

If the R stage fails, upload the handback anyway. It will contain the run log, status file, downloaded raw objects, and any diagnostics produced before failure.


## v1.1 repair

The first v1.0 handback failed before harmonisation because the script attempted to discover the parliamentary-constituency universe using an internal Nomis `TYPE` selector. Nomis returned its API help HTML rather than a CSV data response.

v1.1 removes that dependency. The frozen project input already contains the 571 authoritative ONS/GSS constituency codes, and Nomis supports geography queries by official codes. Each 2011 table is now requested directly for those codes. The script also checks for HTML masquerading as a CSV response and retains every raw response for diagnosis.

No v1.0 empirical result is reused: v1.0 did not reach 2011 harmonisation.


## v1.2 frozen-input discovery repair

v1.1 failed before contacting Nomis because it assumed a retained
`p1_4_a_v1_1` frozen-input directory. The accepted adversarial follow-up on
this same repository used the retained `p1_4_a_v1_0` input successfully.

v1.2 validates candidate frozen inputs against the live application contract.
It requires 571 rows, `ONSConstID`, and all 31 topology variables, writes a
candidate audit, and records the selected file and hash. Without an explicit
`OTWAY_FROZEN_INPUT` override, the retained `p1_4_a_v1_0` candidate is
preferred because it is the reference already used successfully in the
accepted follow-up analysis.


## v1.3 Nomis API schema repair

The v1.2 handback reached the first Nomis download and captured an HTML API
page rather than CSV data. Inspection showed that the request path used
Census table labels such as `C2011QS102EW` as though they were Nomis REST
dataset identifiers. Nomis uses `NM_*` dataset IDs.

v1.3 uses the catalogue-verified mapping:

- QS102EW -> NM_143_1
- QS103EW -> NM_503_1
- KS201EW -> NM_608_1
- KS501EW -> NM_623_1
- KS605EW to KS607EW -> NM_626_1
- KS611EW to KS613EW -> NM_628_1

The analysis-facing table keys are unchanged.

v1.3 also handles the documented schema differences:

- QS103EW classifies age through `C_AGE`, not `CELL`;
- the combined KS605–607 and KS611–613 datasets contain `C_SEX`;
- those combined datasets are requested with `C_SEX=total`;
- classification code/name detection now supports both `CELL` and `C_AGE`.

The complete API contract is written to `03a_nomis_dataset_contract.csv`.

No 2011 empirical values from v1.2 are reused: v1.2 failed on the first
Nomis response before any Census value entered the harmonised frame.


## v1.4 mapping repair and cache reuse

The v1.3 handback successfully downloaded all six official Nomis datasets,
matched 571/571 frozen constituencies, and completed the preflight. Its FAIL
status was caused by two overly strict source-label matchers:

1. QS102EW publishes the density cell as
   `Density (number of persons per hectare)`, not `population density`.
2. KS201EW publishes its denominator as `All usual residents`, not an
   `all categories: ethnic group` label.

v1.4 maps those two published labels explicitly. No category is invented,
combined, or redefined.

v1.4 also reuses the validated raw Nomis batches from the sibling v1.3
`outputs/raw_nomis` directory when present. If the cache is absent or invalid,
the script downloads the relevant batch normally. An alternative cache
directory can be supplied with `OTWAY_NOMIS_CACHE=/path/to/raw_nomis`.

The qualifications and NS-SEC conceptual gates remain deliberately AMBER;
v1.4 does not force either issue to PASS.


## v1.5 conceptual harmonisation resolution

The successful v1.4 run produced a complete 571 x 31 candidate but deliberately
returned AMBER. The review resolves the two flagged issues and one additional
denominator mismatch identified during review.

1. **Qualifications.** The 2011 Apprenticeship category is combined with raw
   Other qualifications in `qualifications_other`. This matches the official
   Census 2021 grouped classification in which apprenticeships are included
   with Other.
2. **Industry.** KS605EW (age 16-74) is replaced by LC6110EW (`NM_1047_1`),
   selecting `C_AGE=total`, i.e. All categories: Age 16 and over. This aligns
   the 2011 population base with Census 2021 industry data.
3. **NS-SEC.** KS611EW (age 16-74) is replaced by LC6114EW (`NM_1051_1`),
   selecting `C_AGE=total`, aligning the 2011 population base with Census 2021
   NS-SEC data.

The first four v1.4 Nomis sources are reused from the validated local cache
where available. Only the two LC tables should normally require new downloads.

v1.5 remains a preflight. A PASS approves the harmonised frame for freezing
and a separately governed 2011 radius/topology stage; it does not reuse
epsilon=5.50.


## v1.6 final density-unit harmonisation

v1.5 passed all programmed gates. Manual review before freeze found that
QS102EW reports density as persons per hectare while the retained 2021
variable register specifies `residents_per_square_km`.

v1.6 multiplies the 2011 density source value by exactly 100 before writing
the 31-coordinate candidate and retains the original persons/hectare value as
a provenance helper. A new explicit gate verifies the conversion.

This positive constant rescaling cannot alter a separately z-standardised
2011 topology, but it ensures the frozen 2011 and 2021 variables use the same
declared unit. A v1.6 `PASS_HARMONISATION_READY_FOR_FREEZE` is intended to be
the final preflight gate before freezing and independent 2011 radius work.
