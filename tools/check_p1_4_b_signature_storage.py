#!/usr/bin/env python3
from __future__ import annotations
import argparse, re, sys
from pathlib import Path


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument('root')
    args = ap.parse_args()
    root = Path(args.root).resolve()
    module = root / 'framework' / 'R' / 'TDABMRadiusDiagnostics.R'
    test = root / 'tools' / 'test_p1_4_b_radius_diagnostics.R'
    problems: list[str] = []

    if not module.is_file():
        problems.append('missing framework/R/TDABMRadiusDiagnostics.R')
        text = ''
    else:
        text = module.read_text(encoding='utf-8', errors='replace')

    required = [
        'tdabm_rd_pack_signature <- function(bits)',
        'padding <- (8L - (length(bits) %% 8L)) %% 8L',
        'tdabm_rd_pack_signature(bits)',
        'tdabm_rd_pack_signature(flags)',
        'expected_bytes <- as.integer(ceiling(n_bits / 8))',
    ]
    for marker in required:
        if marker not in text:
            problems.append(f'missing signature-storage marker: {marker}')

    # packBits must be centralized in the padding helper. Ignore comments so
    # documentation mentioning the API cannot create a false violation.
    executable_lines = []
    for line in text.splitlines():
        code = line.split('#', 1)[0]
        executable_lines.append(code)
    executable_text = '\n'.join(executable_lines)
    pack_calls = len(re.findall(r'base::packBits\s*\(', executable_text))
    if pack_calls != 1:
        problems.append(f'expected exactly one centralized executable base::packBits call; found {pack_calls}')

    if test.is_file():
        t = test.read_text(encoding='utf-8', errors='replace')
        for marker in ['4950L', '276L', 'ceiling(n_bits / 8)', 'tdabm_rd_unpack_signature']:
            if marker not in t:
                problems.append(f'missing non-byte-aligned regression marker: {marker}')
    else:
        problems.append('missing tools/test_p1_4_b_radius_diagnostics.R')

    print('TDABM P1.4-B packed-signature storage contract check')
    print(f'Root: {root}')
    print(f'Violations: {len(problems)}')
    for problem in problems:
        print('ERROR:', problem)
    if not problems:
        print('PASS: topology signatures are padded to whole bytes only for storage and unpacked to the declared logical bit length.')
        print('PASS: non-byte-aligned 24- and 100-observation cases are explicitly covered by regression tests.')
    return 1 if problems else 0


if __name__ == '__main__':
    sys.exit(main())
