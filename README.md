# Electoral Salience without Socio-Demographic Discreteness

Replication code and computational documentation for the Otway–Rudkin analysis
of Labour's Red Wall and electoral realignment in England and Wales.

## Repository policy

**No empirical dataset is distributed here.** Users must obtain and prepare the
required data themselves by following [`DATA_SOURCING.md`](DATA_SOURCING.md).
Raw, processed, frozen, benchmark and observation-level derived files are
excluded from version control.

The repository contains the accepted Ball Mapper framework/application code,
2021 robustness analyses, Census 2011 historical sensitivity, matched
randomisation benchmarks, temporal comparison, v1.6 adversarial-response
evidence code and the authoritative final Figure 3/4 production source.

## Ball Mapper implementation and citation

`framework/BallMapper.cpp` was provided by Paweł Dłotko. Use of that implementation requires citation of:

Dłotko, P. (2019). *Ball mapper: A shape summary for topological data analysis*. arXiv preprint arXiv:1901.07410.

See [`NOTICE.md`](NOTICE.md) for the attribution statement.

## Reproducibility boundary

The recovered project contains the verified analytical contract for the
historical `merged_data.csv`, but not the upstream scripts that originally
assembled that rectangular file from Census, election and referendum sources.
The repository therefore does not fabricate missing provenance.

Two source modes are supported:

- **strict** — the user-supplied `merged_data.csv` matches the historical
  SHA-256 exactly;
- **reconstructed** — the user's lawfully reconstructed source satisfies the
  documented schema but is not byte-identical. In this mode, the canonical
  Ball Mapper topology fingerprint remains a binding replication check.

An input that does not recreate the accepted topology will fail the public
canonical-build gate unless the user explicitly opts into exploratory topology
variation.

## Quick start

```bash
python3 scripts/validate_user_inputs.py
source scripts/prepare_local_inputs.sh
Rscript tools/install_dependencies.R

bash applications/Otway_Rudkin_2024_Corrected31D/scripts/run_p1_4_a.sh
bash scripts/run_public_canonical.sh
```

A successful canonical build must reproduce:

- radius 5.50;
- 28 balls;
- 96 edges;
- 2 connected components;
- 1 isolate;
- topology fingerprint
  `2a7fae54601a98262d7196c0f3ad8c56d43bde998638d95ca24adfffdba4c539`.

Continue with [`RUN_ORDER.md`](RUN_ORDER.md).

## Public-repository safety

`.github/workflows/static-audit.yml` runs a static audit on every push and pull
request. It rejects committed empirical data, large observation-level ONS/GSS
identifier payloads, hard-coded Red Wall source lists and machine-specific
executable paths.

## Documentation

- [`DATA_SOURCING.md`](DATA_SOURCING.md) — required data and construction boundary.
- [`INPUT_CONTRACT.md`](INPUT_CONTRACT.md) — analytical variables and schema.
- [`RUN_ORDER.md`](RUN_ORDER.md) — complete computational sequence.
- [`MANUSCRIPT_CODE_CROSSWALK.md`](MANUSCRIPT_CODE_CROSSWALK.md) — manuscript evidence to code.
- [`REPRODUCIBILITY_BOUNDARY.md`](REPRODUCIBILITY_BOUNDARY.md) — preserved and missing provenance.
