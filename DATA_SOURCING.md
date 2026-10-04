# Data sourcing

## 1. Non-redistribution rule

This repository contains code, configuration and documentation only. Users must
obtain the empirical data themselves. Do not commit raw, processed, frozen,
benchmark or observation-level derived files.

## 2. Main 2021/electoral source

The accepted project used a rectangular file named `merged_data.csv`.

Historical contract:

- 573 candidate constituencies before the analytical exclusions;
- 84 columns in the archived source;
- stable constituency identifier: `ONSConstID`;
- constituency label: `ConstituencyName`;
- CP1252 / Windows-1252 encoding;
- historical SHA-256:
  `79ac20539ee5dc2db7ffae8496996151bc5d7c6a2dd245bf1d8a3f9e958f5643`;
- 571 final constituencies after excluding Chorley and Buckingham because
  Speaker/former-Speaker status makes party vote shares non-comparable.

The source combines:

- Census 2021 constituency socio-demographic characteristics;
- UK general-election results for 2015, 2017 and 2019;
- constituency-level 2016 EU referendum Leave estimates associated with
  Hanretty (2017);
- constituency names, ONS identifiers, country and region fields.

The recovered archive does **not** contain the upstream scripts that originally
assembled the 84-column rectangular file. A new user must therefore construct
or otherwise lawfully obtain an input satisfying
`applications/Otway_Rudkin_2024_Corrected31D/config/application_contract.csv`
and [`INPUT_CONTRACT.md`](INPUT_CONTRACT.md).

Place it at:

`data/user/merged_data.csv`

Run:

```bash
python3 scripts/validate_user_inputs.py
source scripts/prepare_local_inputs.sh
```

If the file is byte-identical to the historical source, strict source mode is
used. Otherwise the preparation script enters reconstructed-source mode. The
accepted canonical topology fingerprint remains binding, so a merely
schema-compatible but substantively different reconstruction will not pass as
a paper replication.

## 3. Published Red Wall definition

Obtain the 42-seat definition from:

Kanagasooriam, J. and Simon, E. (2021), "Red Wall: The Definitive Description",
*Political Insight*, 12(3), 8–11.
DOI: 10.1177/20419058211045127.

Create:

`data/user/redwall_source.csv`

with exactly these columns:

```text
source_constituency,source_region
```

and exactly 42 unique constituency rows. The public code deliberately does not
embed the list. It matches the user-supplied names to the analytical
constituency frame and expects Chorley to be the sole source seat absent from
the final 571-row sample.

## 4. Census 2011 historical sensitivity

The recovered historical-sensitivity code downloads official Census 2011
standard tables from Nomis onto the user's machine. The accepted tables are:

- QS102EW — Population density (`NM_143_1`);
- QS103EW — Age by single year (`NM_503_1`);
- KS201EW — Ethnic group (`NM_608_1`);
- KS501EW — Qualifications and students (`NM_623_1`);
- LC6110EW — Industry by age (`NM_1047_1`);
- LC6114EW — NS-SEC by age (`NM_1051_1`).

The code harmonises them to the same 571-seat ONS-coded frame and 31-coordinate
conceptual schema. Nomis responses and all harmonised observation-level files
remain local.

## 5. Election and referendum variables

The analytical contract requires Conservative and Labour vote-share variables
for 2015, 2017 and 2019, their inter-election changes, 2019 winner information,
and the constituency-level Leave estimate. Exact field names are recorded in
`INPUT_CONTRACT.md` and the application contract.

## 6. Locally generated artefacts

Running the code generates, among other files:

- the deterministic 571-row Corrected31D frozen input;
- radius diagnostics;
- canonical Ball Mapper memberships and overlap edges;
- randomisation benchmark draws;
- harmonised Census 2011 files;
- robustness outputs;
- final figure inputs and images.

All such files are ignored by Git.
