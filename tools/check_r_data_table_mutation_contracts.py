#!/usr/bin/env python3
"""Reject direct [[<- mutation of variables created as data.table objects.

The check is intentionally conservative.  It examines active R source only
(provenance snapshots are excluded), identifies function-local variables whose
assignment expression includes data.table::as.data.table(), and rejects later
base-R column subassignment, names<-, attr<- or class<- on those variables.
Use data.table::set(), setnames() or setattr() instead.
"""
from __future__ import annotations

import argparse
import csv
import re
import sys
from pathlib import Path

FUNC_RE = re.compile(r"(?m)^\s*([A-Za-z.][A-Za-z0-9._]*)\s*<-\s*function\s*\(")
AS_DT_RE = re.compile(
    r"(?m)^\s*([A-Za-z.][A-Za-z0-9._]*)\s*<-\s*.*data\.table::as\.data\.table\s*\("
)


def strip_comments_strings(text: str) -> str:
    out = []
    i = 0
    quote = None
    while i < len(text):
        ch = text[i]
        if quote is not None:
            if ch == "\\" and i + 1 < len(text):
                out.extend("  ")
                i += 2
                continue
            if ch == quote:
                quote = None
            out.append(" ")
            i += 1
            continue
        if ch in ("'", '"'):
            quote = ch
            out.append(" ")
            i += 1
            continue
        if ch == "#":
            while i < len(text) and text[i] != "\n":
                out.append(" ")
                i += 1
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def function_spans(text: str):
    clean = strip_comments_strings(text)
    for match in FUNC_RE.finditer(clean):
        brace = clean.find("{", match.end())
        if brace < 0:
            continue
        depth = 0
        end = None
        for i in range(brace, len(clean)):
            if clean[i] == "{":
                depth += 1
            elif clean[i] == "}":
                depth -= 1
                if depth == 0:
                    end = i + 1
                    break
        if end is not None:
            yield match.group(1), match.start(), end, clean[match.start():end]


def scan_file(path: Path, root: Path):
    text = path.read_text(encoding="utf-8", errors="replace")
    rows = []
    for function_name, start, end, body in function_spans(text):
        variables = sorted(set(AS_DT_RE.findall(body)))
        for var in variables:
            patterns = {
                "direct_double_bracket_assignment": re.compile(
                    rf"\b{re.escape(var)}\s*\[\[[^\]]+\]\]\s*<-"
                ),
                "names_assignment": re.compile(rf"names\s*\(\s*{re.escape(var)}\s*\)\s*<-"),
                "attr_assignment": re.compile(
                    rf"attr\s*\(\s*{re.escape(var)}\s*,[^)]*\)\s*<-"
                ),
                "class_assignment": re.compile(rf"class\s*\(\s*{re.escape(var)}\s*\)\s*<-"),
            }
            for violation_type, pattern in patterns.items():
                for hit in pattern.finditer(body):
                    absolute = start + hit.start()
                    line = text.count("\n", 0, absolute) + 1
                    rows.append({
                        "file": str(path.relative_to(root)),
                        "line": line,
                        "function": function_name,
                        "variable": var,
                        "violation_type": violation_type,
                        "status": "violation",
                    })
    return rows


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("root")
    parser.add_argument("--output", default=None)
    args = parser.parse_args()
    root = Path(args.root).resolve()
    files = [
        p for p in root.rglob("*.R")
        if "provenance" not in p.parts and "__pycache__" not in p.parts
    ]
    rows = []
    for path in sorted(files):
        rows.extend(scan_file(path, root))
    output = Path(args.output) if args.output else root / "manifests/P1_3_4_3_data_table_mutation_contract.csv"
    output.parent.mkdir(parents=True, exist_ok=True)
    fields = ["file", "line", "function", "variable", "violation_type", "status"]
    with output.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
    print("TDABM P1.3.4 v1.0.3 data.table mutation contract check")
    print(f"Active R files scanned: {len(files)}")
    print(f"Violations: {len(rows)}")
    print(f"Report: {output}")
    if rows:
        for row in rows[:20]:
            print(f"ERROR: {row['file']}:{row['line']} {row['function']} mutates {row['variable']} via {row['violation_type']}")
        return 1
    print("PASS: active data.table objects use set-family mutation rather than base column or attribute replacement.")
    return 0

if __name__ == "__main__":
    sys.exit(main())
