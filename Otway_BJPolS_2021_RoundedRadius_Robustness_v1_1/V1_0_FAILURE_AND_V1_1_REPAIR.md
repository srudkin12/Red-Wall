# v1.0 failure and v1.1 repair

The v1.0 handback successfully verified the accepted production evidence:

- P1.4-B ZIP SHA-256 matched the retained accepted hash;
- the archive contained the exact 386-radius grid from 0.75 to 20.00 by 0.05;
- grid row 96 was 5.50;
- canonical validation reproduced 28 balls, 96 edges, 2 components, 1 isolate.

The run then failed at the first ILR radius because the patched selector used
two wrong helper names. v1.1 restores the original helper contract:
`summary_numeric()` plus `topology_loss_row()`.

No empirical ILR/LOBO result from the failed v1.0 run is carried forward.
