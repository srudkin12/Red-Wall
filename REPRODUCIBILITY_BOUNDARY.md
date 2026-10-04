# Reproducibility boundary

Recovery of the complete project established the following provenance boundary.

## Preserved

The repository retains source code for:

- Corrected31D sample construction and input freezing;
- topology-only radius diagnostics;
- canonical Ball Mapper construction;
- targeted 2021 robustness and calibration;
- Census 2011 acquisition/harmonisation;
- 2011 radius governance and canonical analysis;
- independent single-core verification;
- matched 2011 benchmarks and paired 2011→2021 contrasts;
- v1.6 prevalence/mixing calibration and graph-neighbourhood evidence;
- authoritative final Figure 3/4 production.

## Not preserved

The upstream scripts that originally assembled the historical 84-column
`merged_data.csv` from Census 2021, election and referendum sources are not
present in the recovered archive. The public repository therefore requires
users to source/construct that input according to the verified analytical
contract.

A separate authoritative production script for manuscript Figure 1 and the
original Figure 2 rendering was also not found. Their underlying election data
and canonical topology are part of the documented analytical contract, but the
repository does not fabricate missing plotting code.

## Public reconstruction safeguard

A byte-different reconstructed `merged_data.csv` can enter the pipeline in
`OTWAY_SOURCE_MODE=reconstructed`, but the public canonical builder requires the
accepted topology fingerprint by default. This distinguishes honest source
reconstruction from a changed empirical analysis.


Public repository: https://github.com/srudkin12/Red-Wall
