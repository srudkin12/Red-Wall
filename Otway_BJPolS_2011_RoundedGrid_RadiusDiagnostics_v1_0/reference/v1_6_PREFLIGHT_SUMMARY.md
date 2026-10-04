# 2011 Census preflight / harmonisation summary

Generated: 2026-10-02 23:56:51 BST

## Scope
This run is a feasibility and harmonisation preflight only. It does not construct a 2011 Ball Mapper topology or select a 2011 radius.

## Computational gates
- Direct Nomis retrieval of the frozen 571 GSS-code frame across all six tables: **PASS**.
- Frozen 571 constituency GSS-code match in QS102EW: **PASS** (571/571).
- Category mapping: **PASS**.
- 31-coordinate construction: **PASS**.
- Complete 571 x 31 candidate frame: **PASS**.

## Resolved harmonisation gates
- Density units: **PASS**. QS102EW persons/hectare is converted to residents/km^2 by x100; 2011 harmonised mean 2134.151 versus frozen 2021 mean 2267.931.
- Qualifications: **PASS**. 2011 Apprenticeship is combined with raw Other to match the official grouped cross-census definition. Harmonised six-coordinate sums: 2011 median 100.000; 2021 median 100.000.
- Industry denominator: **PASS**. LC6110EW supplies the 2011 age-16+ employed population base.
- NS-SEC denominator: **PASS**. LC6114EW supplies the 2011 age-16+ population base.

## Overall
Overall preflight status: **PASS_HARMONISATION_READY_FOR_FREEZE**.

If PASS, the harmonised 2011 571 x 31 frame is approved for a separate freeze and topology-governance stage. The 2011 radius must still be selected independently from topology-only diagnostics.

## Production rule if the preflight is approved later
A subsequent 2011 Ball Mapper analysis must govern its radius independently using topology-only diagnostics. It must not import epsilon = 5.50 from the 2021 Corrected31D topology, and graph identity must not be used as the success criterion.
