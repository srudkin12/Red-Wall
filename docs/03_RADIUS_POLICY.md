# Radius Policy

TDABM portability does not claim a universal topology-only optimum.

The current diagnostic architecture is designed to prevent systematic
coarsening.

## Report, do not hard-gate

Report:

- actual ball count;
- effective ball count;
- ball-size distribution;
- largest-ball share;
- components;
- isolated balls;
- overlap/multicoverage;
- cycle/edge structure where available;
- repeated-order cover stability;
- repeated-order landmark stability;
- radius divided by square root of dimension count as an interpretive scale
  where appropriate.

Connectedness and absence of isolates are descriptive transition markers only.

## Non-binding anchors

- 40% largest-ball dominance transition;
- 50% majority-cover transition — primary pipeline reference;
- 60% dominance transition;
- 75% severe-coarsening marker.

The 40%–60% range is a review region, not an admissibility theorem.

## User decision

The final radius remains an explicit substantive decision. Record the pipeline
reference and the user-approved value separately.

Colour/outcome variables may not be used to choose the radius.


## Contract semantics

The review region is non-binding. A valid explicit user-radius contract may record `BELOW_REVIEW_BAND`, `INSIDE_REVIEW_BAND`, or `ABOVE_REVIEW_BAND`. Blocking integrity failures (malformed, non-finite, mismatched or invalid-provenance evidence) are distinct from non-blocking substantive diagnostics such as dominance, low ball count, connectivity, isolates, or review-band location. Only the explicit user decision approves an application radius.
