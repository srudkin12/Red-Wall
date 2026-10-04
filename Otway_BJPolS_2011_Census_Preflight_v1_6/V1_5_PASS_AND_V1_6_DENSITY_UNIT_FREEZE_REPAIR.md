# v1.5 PASS and v1.6 density-unit freeze repair

v1.5 returned `PASS_HARMONISATION_READY_FOR_FREEZE`.

Before freezing, manual review of the cross-census diagnostics found the
remaining declared-unit mismatch:

- 2011 QS102EW: persons per hectare; mean 21.3415.
- frozen 2021 variable register: residents per square kilometre; mean 2267.9313.

v1.6 applies the exact x100 conversion to the 2011 density coordinate. The
expected 2011 harmonised mean is 2134.1506 residents/km^2. The observed
2011/2021 constituency correlation (~0.996) and any within-census
z-standardised Ball Mapper geometry are unchanged.

No other v1.5 harmonisation decision is modified.
