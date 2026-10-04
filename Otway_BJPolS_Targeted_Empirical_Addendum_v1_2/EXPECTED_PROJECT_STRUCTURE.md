# Expected project structure

The addendum discovers and checks the following existing objects automatically.

```text
TDABM_Portability/
├── framework/
│   └── R/
│       └── TDABMRadiusDiagnostics.R
└── applications/
    └── Otway_Rudkin_2024_Corrected31D/
        ├── R/
        │   └── OtwayRudkin2024Adapter.R
        ├── config/
        │   ├── application_contract.csv
        │   └── p1_4_c_radius_decision.csv
        ├── frozen_input/
        │   └── p1_4_a_v1_0/
        │       └── Otway_Rudkin_2024_Corrected31D_input_frozen_P1_4_A_v1_0.csv
        └── frozen_topology/
            └── p1_4_c_v1_0/
                ├── canonical_point_order.csv
                ├── canonical_memberships.csv
                └── ...
```

The code also searches the wider repository for the Red Wall lookup introduced during
the later P1.4-D/E/F evidence stages. It requires exactly 41 Red Wall constituencies
within the 571-row analytical sample.

The primary Corrected31D radius remains 5.50. The addendum does not reopen that decision.


## Red Wall labels

The addendum does not require a historical project Red Wall lookup file. The handover records that
lookup as unresolved. v1.2 reconstructs the manuscript definition directly from the published
Kanagasooriam & Simon (2021) 42-seat list and requires exactly 41 matches after the accepted Chorley
exclusion.
