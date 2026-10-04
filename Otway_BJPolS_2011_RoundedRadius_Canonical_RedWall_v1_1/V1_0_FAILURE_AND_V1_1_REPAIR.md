# v1.0 failure and v1.1 repair

## Failure

v1.0 stopped before topology construction at the Red Wall source-contract
stage:

`Expected 41 unique matched Red Wall ONS IDs.`

## Diagnosis

The embedded accepted audit is correct. It contains 42 source rows, of which
41 have distinct matched ONS IDs and Chorley alone is unmatched.

The `matched` field is serialised as `True` / `False`. R may retain these
title-case strings as character values. The v1.0 test only recognised
`TRUE` / `1`, so it classified every source row as unmatched.

## Repair

v1.1 explicitly normalises the flag to lower case and accepts
`true`, `t`, `1`, `yes`, or `y`.

Before topology construction, it writes:

- `00a_redwall_source_flag_parse_audit.csv`;
- `00b_redwall_source_contract.csv`.

The source contract must pass all of:

1. 42 source rows;
2. 41 matched rows;
3. 41 unique matched ONS IDs;
4. one unmatched row;
5. Chorley is the sole unmatched source;
6. all 41 matched ONS IDs occur in the frozen 2011 frame.

No empirical result from the failed v1.0 execution is carried forward.
Radius epsilon = 5.75 and all corrected rounded-grid governance remain
unchanged.
