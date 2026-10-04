# v1.4 AMBER review and v1.5 resolution

v1.4 passed every computational gate:
- all six Nomis sources retrieved;
- 571/571 constituency codes matched;
- zero category-mapping failures;
- 31/31 coordinates constructed;
- complete 571 x 31 frame.

The AMBER review found:

1. **Qualifications:** 2011 six raw categories summed to 96.176% at the median
   because Apprenticeship was separate (median 3.824%). The frozen 2021 six
   coordinates summed to 100%. ONS Census 2021 classification 7a explicitly
   groups apprenticeships with Other. v1.5 therefore defines
   `qualifications_other = apprenticeship + other`.

2. **NS-SEC:** v1.4 used KS611EW, whose population base is age 16-74. Census
   2021 TS062 uses age 16+. v1.5 uses LC6114EW (NM_1051_1), selecting the
   all-age-16+ total.

3. **Industry:** review identified the same issue even though v1.4 had not
   flagged it. KS605EW is age 16-74, while Census 2021 TS060 is age 16+ in
   employment. v1.5 uses LC6110EW (NM_1047_1), selecting the all-age-16+
   total.

No Ball Mapper topology is constructed by this package. A v1.5 PASS means
the 2011 harmonised input can be frozen and passed to an independently
governed radius/topology stage.
