# 2011 rounded-radius canonical Red Wall sensitivity v1.1

The corrected 386-radius production screen supports **epsilon = 5.75**.

Selection rule: 5.75 is the earliest rounded 0.05-grid radius for which the
central 90% landmark-order distribution is wholly 2 components / 1 isolate.
Across all 1,000 production runs, 977 are exactly 2 components / 1 isolate;
the other 23 are minor 3-component / 2-isolate fragmentations.

The previous long-decimal radius 5.802467853817 is superseded and is not used.

This package:

1. verifies the frozen 571 x 31 harmonised 2011 input;
2. reconstructs the deterministic canonical topology at 5.75;
3. requires the corrected production summary of 26 balls, 94 edges,
   2 components and 1 isolate;
4. freezes and hashes canonical topology objects;
5. introduces the accepted 42-seat Red Wall source audit, yielding 41 matched
   seats with Chorley as the sole missing source seat;
6. calculates canonical Red Wall mixing/discreteness diagnostics;
7. repeats those diagnostics over 1,000 random landmark orders;
8. compares invariant-level diagnostics with the accepted 2021 canonical
   results.

## Run

```bash
cd /path/to/otway-rudkin-red-wall-ball-mapper
unzip -o Otway_BJPolS_2011_RoundedRadius_Canonical_RedWall_v1_0.zip
cd Otway_BJPolS_2011_RoundedRadius_Canonical_RedWall_v1_0
chmod +x run_all.sh
./run_all.sh
```

Upload the generated handback ZIP and `.sha256`.


## v1.1 Red Wall flag-parsing repair

The first v1.0 execution stopped before topology construction with:

`Expected 41 unique matched Red Wall ONS IDs.`

Inspection of the embedded accepted source audit confirms that the source is
correct:

- 42 source constituencies;
- 41 matched rows;
- 41 unique matched ONS IDs;
- Chorley is the sole unmatched source seat.

The failure was an R parsing issue. The CSV serialises the Boolean field as
title-case `True` / `False`; the v1.0 script only accepted `TRUE` / `1`.
v1.1 normalises the field with `tolower(trimws(as.character(...)))` and
requires an explicit six-gate Red Wall source contract before proceeding.

No radius, topology, Census input or Red Wall definition has changed.
Canonical epsilon remains **5.75**.
