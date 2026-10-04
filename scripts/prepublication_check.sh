#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

echo "== Static repository audit =="
python3 scripts/static_repo_audit.py

echo
echo "== Bash syntax =="
while IFS= read -r -d '' f; do
  bash -n "$f"
done < <(find . -type f -name '*.sh' -print0)

echo
echo "== Python syntax =="
python3 - <<'PY'
from pathlib import Path
root=Path(".")
bad=[]
for p in root.rglob("*.py"):
    try:
        compile(p.read_text(encoding="utf-8"),str(p),"exec")
    except Exception as e:
        bad.append((str(p),repr(e)))
if bad:
    for x in bad: print("FAIL",x)
    raise SystemExit(1)
print("PASS: Python sources parse")
PY

echo
echo "== Required repository files =="
for f in README.md DATA_SOURCING.md INPUT_CONTRACT.md RUN_ORDER.md MANUSCRIPT_CODE_CROSSWALK.md REPRODUCIBILITY_BOUNDARY.md CITATION.cff; do
  test -s "$f" || { echo "Missing or empty: $f" >&2; exit 1; }
done

echo
echo "PASS: prepublication repository checks"
