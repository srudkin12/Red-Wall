# v1.3 FAIL and v1.4 mapping repair

## What v1.3 achieved

The v1.3 run completed normally and reached the final preflight decision.

Passed:
- all six Nomis datasets retrieved;
- 571/571 frozen constituencies returned by every dataset;
- 571/571 constituency GSS-code match;
- industry mapping;
- age single-year reconstruction;
- qualifications source-category detection;
- NS-SEC source-category detection.

The final status was FAIL because exactly two category mappings failed,
leaving 29/31 coordinates complete.

## Two failed mappings

From `05_nomis_cell_catalogue.csv`:

### QS102EW
- CELL 0: All usual residents
- CELL 1: Area Hectares
- CELL 2: Density (number of persons per hectare)

The v1.3 matcher incorrectly searched for the phrase `population density`.

### KS201EW
- CELL 0: All usual residents
- CELL 1: White: English/Welsh/Scottish/Northern Irish/British

The v1.3 denominator matcher incorrectly required either `ethnic` or
`all categories`.

## v1.4 repair

v1.4 explicitly matches the published labels:

- `^density number of persons per hectare$`
- `^all usual residents$`

No substantive harmonisation decision is changed.

The two conceptual gates remain AMBER:
1. 2011 apprenticeships are a separate qualification category, while the
   frozen 2021 six qualification coordinates sum to 100%.
2. KS611EW uses the 16–74 population; the exact frozen 2021 NS-SEC source
   denominator still requires verification.

A computational PASS should therefore lead to
`AMBER_CONCEPTUAL_REVIEW_REQUIRED`, not automatic approval of a 2011
Ball Mapper production run.
