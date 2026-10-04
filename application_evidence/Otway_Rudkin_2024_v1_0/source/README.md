# Local empirical source location

`merged_data.csv` is deliberately not distributed.

Run `source scripts/prepare_local_inputs.sh` from the repository root (or follow
`DATA_SOURCING.md`). The preparation script copies the user's local
`data/user/merged_data.csv` into this directory because the preserved P1.4
application code expects that historical location.

Earlier exploratory `bmc*.R` and `dataprep.R` helper files are not part of the
accepted manuscript pipeline and are omitted from the public repository.
