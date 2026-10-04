#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
Rscript --vanilla "$ROOT/scripts/build_public_canonical.R" "$ROOT"
