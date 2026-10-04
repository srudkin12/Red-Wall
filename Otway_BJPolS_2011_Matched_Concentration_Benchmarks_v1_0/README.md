# 2011 matched concentration benchmarks v1.0

## Purpose

The 2011 historical sensitivity already establishes non-discreteness:

- canonical epsilon = 5.75;
- 26 balls, 94 edges, 2 components, 1 isolate;
- 41 matched Red Wall constituencies;
- no majority or exclusive Red Wall ball;
- those conclusions survive 1,000 landmark orders and an independent
  single-core reproducibility audit.

The remaining temporal question is **concentration**. This package subjects
the fixed 2011 canonical graph to the same four 10,000-draw benchmark
contracts used in the accepted 2021 analysis:

1. unrestricted 41-seat labels;
2. same regional composition;
3. same regional composition with eligibility restricted to seats where
   Labour's 2017 vote share exceeded Conservative's;
4. region × 2017 Labour-Conservative margin quartile.

## Strong matched design

The 2011 Red Wall label audit and the accepted 2021 electoral-history audit
have the same 571 ONS constituency IDs in exactly the same row order.

The package therefore uses the same benchmark seeds and same sequential
sampling algorithm as the 2021 analysis. Corresponding benchmark draw numbers
apply the same random label assignment to the 2011 and 2021 canonical graphs.
The paired output can therefore isolate how the different Census topology
changes the benchmark diagnostics.

## Run

```bash
cd /path/to/otway-rudkin-red-wall-ball-mapper

unzip -o Otway_BJPolS_2011_Matched_Concentration_Benchmarks_v1_0.zip

cd Otway_BJPolS_2011_Matched_Concentration_Benchmarks_v1_0

chmod +x run_all.sh

./run_all.sh
```

Default: 10,000 draws for each of four benchmark contracts.

Upload the generated handback ZIP and `.sha256`.
