# v1.1 failure and v1.2 repair

The uploaded v1.1 handback has SHA-256:

`5be2413347f2b486d63384581ad8a097b607c3d8b584db217991d54695159905`

v1.1 exited before any Nomis request because it looked for a nonexistent
`p1_4_a_v1_1` frozen input. The accepted adversarial-followup code on the
same repository used the retained `p1_4_a_v1_0` input successfully.

v1.2 discovers and validates candidate P1.4-A frozen inputs. A candidate is
accepted only if it has exactly 571 rows, contains `ONSConstID`, and contains
all 31 topology variables from the application contract. The complete
candidate audit and selected file hash are written to the handback.

No empirical output from v1.1 is reused because v1.1 never reached Census
download or harmonisation.
