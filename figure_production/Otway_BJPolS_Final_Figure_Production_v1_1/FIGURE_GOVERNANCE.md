# Final figure governance

Figures 3 and 4 are generated from the accepted P1.4-C Corrected31D topology,
the frozen 571-row 2021 input, and the accepted 41-seat Red Wall match.

No radius, topology, Red Wall definition or electoral result is re-estimated.

Figure 3 uses the full 0-1 prevalence scale for Red Wall share and marks 0.5
as the majority threshold.

Figure 4 uses one common symmetric zero-centred scale for both vote-change
panels. Its absolute limit is the smallest integer ceiling covering both
observed change ranges.

Node coordinates are fixed from the current manuscript Figure 2. No random
layout algorithm is run, preserving visual comparability across Figures 2-4.

The code fails closed if the frozen input checksum, topology fingerprint,
28-ball/96-edge structure, 41-seat Red Wall match, published Figure 4 ranges,
or top-ten left-behind change statement do not reproduce.


## v1.1 export-QA correction

v1.0 produced correct scales and values but the right-hand colour-bar tick labels were clipped by the raster export canvas. v1.1 uses explicit tick labels and tight bounding boxes with padding. No empirical data, topology, scale or manuscript text changes.
