# 2011 rounded-grid radius diagnostics v1.0

This package supersedes the 2011 radius chain that used 61 equally spaced
long-decimal radii and promoted epsilon 5.802467853817.

The harmonised 2011 Census input remains valid. Only the radius/topology chain
is replaced.

Authoritative production contract:

- 0.75 to 20.00 inclusive;
- 0.05 increments;
- 386 two-decimal radii;
- 1,000 landmark-order repetitions per radius;
- 50 workers where available;
- z-score scaling and Euclidean distance;
- topology-only diagnostics;
- no automatic radius selection.

Run:

```bash
cd /path/to/otway-rudkin-red-wall-ball-mapper
unzip -o Otway_BJPolS_2011_RoundedGrid_RadiusDiagnostics_v1_0.zip
cd Otway_BJPolS_2011_RoundedGrid_RadiusDiagnostics_v1_0
chmod +x run_all.sh
./run_all.sh
```

Upload the generated handback ZIP and `.sha256`.
