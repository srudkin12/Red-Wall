# 2011–2021 temporal concentration contrast v1.0

This is a deterministic analysis of the already completed matched benchmark
draws. It does not generate new random labels.

The primary temporal concentration measure is the manuscript's existing
concentration measure: **number of occupied Red Wall balls**.

Observed change:

- 2011: 14 occupied balls;
- 2021: 11 occupied balls;
- concentration gain: 3 fewer occupied balls in 2021.

For each of the four benchmark contracts, the script compares that observed
3-ball change with the 10,000 paired random-label changes obtained by applying
the same label draw to the 2011 and 2021 canonical topologies.

Secondary checks report changes in membership entropy and maximum within-ball
Red Wall prevalence, but do not replace the pre-specified primary measure.

Run:

```bash
cd /path/to/otway-rudkin-red-wall-ball-mapper
unzip -o Otway_BJPolS_2011_2021_Temporal_Concentration_Contrast_v1_0.zip
cd Otway_BJPolS_2011_2021_Temporal_Concentration_Contrast_v1_0
chmod +x run_all.sh
./run_all.sh
```

The run should be quick because the 40,000 paired benchmark draws are already
embedded. Upload the resulting handback ZIP and `.sha256`.
