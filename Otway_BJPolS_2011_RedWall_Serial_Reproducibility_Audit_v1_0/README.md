# 2011 Red Wall serial reproducibility audit v1.0

The canonical Red Wall sensitivity run completed unusually quickly on the
multi-core Linux machine. Its handback already demonstrates 1,000 distinct
replicate IDs, 1,000 distinct seeds and 1,000 distinct metric rows.

This package performs a stronger independent check:

- recomputes the exact same 1,000 seeds;
- forces `workers = 1`, so no forked parallel execution is used;
- uses the same frozen 571 x 31 input, Red Wall audit and original function
  definitions;
- compares every output column and every replicate against the original
  parallel handback;
- requires numeric agreement within 1e-12 and exact agreement for
  non-numeric fields.

Run:

```bash
cd /path/to/otway-rudkin-red-wall-ball-mapper
unzip -o Otway_BJPolS_2011_RedWall_Serial_Reproducibility_Audit_v1_0.zip
cd Otway_BJPolS_2011_RedWall_Serial_Reproducibility_Audit_v1_0
chmod +x run_all.sh
./run_all.sh
```

This run should be noticeably slower than the original 50-core execution
because all 1,000 repetitions are deliberately run on one worker.

Upload the timestamped handback ZIP and `.sha256`.
