# Source and harmonisation notes

## Official 2011 source route

The preflight uses the Office for National Statistics 2011 Census outputs served through Nomis. ONS directs users of historic 2011 Census data to Nomis, and ONS specifically released Key Statistics and Quick Statistics for 2010 Westminster parliamentary constituencies in England and Wales.

The package uses these standard 2011 tables:

| Frozen concept | 2011 source | Preflight treatment |
|---|---|---|
| Population density | QS102EW | Direct density cell; unit retained in provenance |
| Age | QS103EW | Single-year ages aggregated exactly to 0–15, 16–24, 25–34, 35–49, 50–64, 65+ |
| White British ethnicity | KS201EW | Direct share of the White British category |
| Highest qualification | KS501EW | No qualification, Levels 1–4+, Other; apprenticeship retained separately for audit and not silently reassigned |
| Industry | KS605EW | Published SIC-based categories aggregated to the eight frozen industry coordinates |
| NS-SEC | KS611EW | Nine top-level frozen categories; nested subcategories excluded from double counting |

## Why the 2011 constituency frame should be tractable

The empirical paper uses 2015–2019 Westminster election constituencies. Nomis exposes the 2010 parliamentary-constituency geography for the 2011 Census. The preflight therefore matches the project's frozen constituency IDs directly against the official GSS constituency codes returned by Nomis rather than relying on constituency-name matching.

The frozen project contains 571 constituencies. The official England-and-Wales 2010 parliamentary-constituency universe is expected to be larger because the analytical frame excludes constituencies that cannot enter the electoral calculations used by the project. The preflight does not assume which constituencies these are: it reports the unmatched extras explicitly.

## Deliberate non-decisions

This package does not decide any of the following automatically:

1. whether the six frozen qualification coordinates intentionally excluded the separate apprenticeship category in 2021;
2. whether the 2021 NS-SEC denominator is exactly comparable with the 2011 KS611EW population aged 16–74;
3. whether a completed 2011 candidate frame should use the same Ball Mapper radius as 2021 (it should not be assumed);
4. whether the 2011 and 2021 topologies should be graph-identical (they should not be required to be).

A later production package should be written only after the handback from this preflight has been audited.


## v1.1 geography retrieval change

The production preflight no longer attempts to reconstruct the full Nomis parliamentary-constituency universe using an internal geography type code. It submits the 571 frozen `ONSConstID` GSS codes directly to each of the six official 2011 Census datasets and requires exact coverage in every table. This makes the 2021 frozen analytical frame, rather than a potentially changing Nomis internal hierarchy, the authoritative matching frame.


## v1.2 frozen 2021 reference

Candidate P1.4-A inputs are now validated rather than selected by a hard-coded
sub-version path. This repair changes only source discovery and provenance;
it does not change the frozen 2021 analysis or any 2011 harmonisation rule.


## v1.3 Nomis dataset identifiers

Nomis distinguishes the human-readable Census table identifier from the REST
API dataset identifier. The preflight now records both. It also treats
QS103EW's `C_AGE` dimension as the source classification and filters the
combined industry/NS-SEC datasets to total sex before category mapping.

These changes affect data acquisition/schema interpretation only. The target
31-variable harmonisation specification is unchanged.


## v1.4 mapping evidence

The v1.3 source catalogue established the exact labels used for the two
previous FAIL rows:

- QS102EW CELL 2: `Density (number of persons per hectare)`;
- KS201EW CELL 0: `All usual residents`.

v1.4 therefore changes only those two matching rules. The White British
numerator remains the published category
`White: English/Welsh/Scottish/Northern Irish/British`.

The deliberate qualifications/apprenticeship and NS-SEC denominator review
gates are unchanged.


## v1.5 AMBER review resolution

External definition checks establish:

- Census 2021 highest-level-of-qualification classification 7a combines
  apprenticeships with Other. The v1.5 2011 construction uses the same grouping.
- Census 2021 industry estimates classify residents aged 16+ in employment.
  LC6110EW is used because it provides the corresponding 2011 age-16+ base.
- Census 2021 NS-SEC estimates classify usual residents aged 16+.
  LC6114EW is used because it provides the corresponding 2011 age-16+ base.

The earlier KS605EW and KS611EW age-16-to-74 sources are retained in prior
handbacks for provenance but are not used in the v1.5 harmonised candidate.


## v1.6 density unit resolution

QS102EW publishes density in persons per hectare. The frozen 2021 variable
register records `population_density` as residents per square kilometre.
v1.6 applies:

`residents_per_square_km = persons_per_hectare * 100`

The raw 2011 source value is retained in
`__population_density_persons_per_hectare`; only the converted
`population_density` enters the harmonised 31-coordinate frame.
