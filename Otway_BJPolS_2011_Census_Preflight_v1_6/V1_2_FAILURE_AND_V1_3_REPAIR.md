# v1.2 failure and v1.3 repair

## Verified v1.2 failure point

v1.2 successfully located the authoritative retained frozen 2021 input:

`applications/Otway_Rudkin_2024_Corrected31D/frozen_input/p1_4_a_v1_0/Otway_Rudkin_2024_Corrected31D_input_frozen_P1_4_A_v1_0.csv`

It then failed on the first external request. The retained raw file begins
with the Nomis HTML API page rather than CSV data.

## Root cause

The request used the Census table label (`C2011QS102EW`) in the URL path.
Nomis REST delivery requires the dataset's `NM_*` identifier.

Catalogue-verified v1.3 mapping:

| Census table | Nomis API dataset |
|---|---|
| QS102EW | NM_143_1 |
| QS103EW | NM_503_1 |
| KS201EW | NM_608_1 |
| KS501EW | NM_623_1 |
| KS605EW to KS607EW | NM_626_1 |
| KS611EW to KS613EW | NM_628_1 |

QS103EW uses `C_AGE` rather than `CELL`. The combined industry and NS-SEC
datasets also use `C_SEX`; v1.3 explicitly requests the total-sex category.

No Census observation from v1.2 is reused because the run stopped at the
first response.
