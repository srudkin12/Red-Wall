# 2021 rounded-radius robustness repair v1.1

The canonical 2021 radius remains the researcher-approved **5.50**.

Final P1.4-B production used the common rounded grid:

- 0.75 to 20.00 inclusive;
- 0.05 increments;
- 386 radii;
- 1,000 repetitions in final production;
- 50 workers.

This package verifies the retained P1.4-B audit when available and re-runs the
later ILR and leave-one-block-out sensitivity radius searches on that same
rounded 386-value grid. It removes the long-decimal sensitivity radii generated
by the earlier adversarial follow-up.

Run:

```bash
cd /path/to/otway-rudkin-red-wall-ball-mapper
unzip -o Otway_BJPolS_2021_RoundedRadius_Robustness_v1_0.zip
cd Otway_BJPolS_2021_RoundedRadius_Robustness_v1_0
chmod +x run_all.sh
./run_all.sh
```

Upload the generated handback ZIP and `.sha256`.


## v1.1 code repair

The first rounded-radius rerun verified the accepted P1.4-B production audit
and exact 386-radius grid successfully, but stopped at ILR grid point 0.75.

The failure was a patching error: the v1.0 replacement selector called
`topology_target()` and `topology_loss()`, whereas the accepted adversarial
code defines `summary_numeric()` and `topology_loss_row()` for this step.

v1.1 corrects those function calls only. The radius governance is unchanged:

- canonical 2021 radius remains 5.50;
- alternative-geometry radius searches use 0.75–20.00 by 0.05;
- all selected sensitivity radii must be two-decimal grid values.
