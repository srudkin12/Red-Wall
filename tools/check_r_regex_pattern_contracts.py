#!/usr/bin/env python3
"""Reject active R code that constructs a dynamic regex directly inside grepl(paste0(...)).

P1.3.4 v1.0.4 rule: dynamic pattern vectors must be evaluated element-wise through
shared claim/pattern contracts. Provenance snapshots are excluded because they are immutable.
"""
from __future__ import annotations
import argparse
import csv
import re
from pathlib import Path

FORBIDDEN = re.compile(r"\b(?:grepl|grep)\s*\(\s*paste0\s*\(", re.MULTILINE)
EXCLUDED_PARTS = {"provenance", ".git", "renv", "packrat"}


def active_r_files(root: Path):
    for path in root.rglob("*.R"):
        if any(part in EXCLUDED_PARTS for part in path.relative_to(root).parts):
            continue
        yield path


def strip_comments(text: str) -> str:
    # Preserve strings because the forbidden construct itself is code-level and the
    # framework currently contains no generated R source strings. Remove comments
    # conservatively while respecting quoted strings.
    out = []
    for line in text.splitlines(True):
        quote = None
        escaped = False
        cut = len(line)
        for i, ch in enumerate(line):
            if escaped:
                escaped = False
                continue
            if ch == "\\":
                escaped = True
                continue
            if quote:
                if ch == quote:
                    quote = None
                continue
            if ch in ("'", '"'):
                quote = ch
                continue
            if ch == "#":
                cut = i
                break
        out.append(line[:cut] + ("\n" if line.endswith("\n") and cut < len(line) else ""))
    return "".join(out)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("root", type=Path)
    ap.add_argument("--output", type=Path, default=None)
    args = ap.parse_args()
    root = args.root.resolve()
    violations = []
    scanned = 0
    for path in active_r_files(root):
        scanned += 1
        text = strip_comments(path.read_text(encoding="utf-8", errors="replace"))
        for m in FORBIDDEN.finditer(text):
            line = text.count("\n", 0, m.start()) + 1
            violations.append({
                "relative_path": str(path.relative_to(root)),
                "line": line,
                "rule": "dynamic_pattern_inside_grep",
                "detail": "Use an element-wise shared pattern/claim contract instead of grep/grepl(paste0(...)).",
            })
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        with args.output.open("w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=["relative_path", "line", "rule", "detail"])
            w.writeheader(); w.writerows(violations)
    print("TDABM P1.3.4 v1.0.4 regex-pattern contract check")
    print(f"Root: {root}")
    print(f"Active R files scanned: {scanned}")
    print(f"Violations: {len(violations)}")
    for v in violations:
        print(f"ERROR: {v['relative_path']}:{v['line']}: {v['detail']}")
    if not violations:
        print("PASS: no active R code constructs a dynamic regex directly inside grep/grepl(paste0(...)).")
    return 2 if violations else 0

if __name__ == "__main__":
    raise SystemExit(main())
