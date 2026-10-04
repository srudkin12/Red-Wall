#!/usr/bin/env python3
from pathlib import Path
import argparse, hashlib, re, sys

REQUIRED = [
    "README.md",
    "NEW_CONVERSATION_HANDOVER.md",
    "VERSION",
    "framework/BallMapper.cpp",
    "framework/BMCLAUDE3.R",
    "framework/coref.R",
    "framework/R/TDABMInputFreeze.R",
    "framework/R/TDABMRadiusDiagnostics.R",
    "framework/R/TDABMTopologyFreeze.R",
    "framework/R/TDABMFrozenTopology.R",
    "framework/R/TDABMPaperEvidence.R",
    "framework/R/TDABMInterpretationRobustness.R",
    "framework/R/TDABMEvidenceProvenance.R",
    "templates/new_application/config/application_contract.csv",
]
FORBIDDEN_TREE_PARTS = {
    "outputs", "local_runs", "p1_acceptance", "__pycache__", ".git"
}
FORBIDDEN_FILE_SUFFIXES = {".zip", ".sha256"}
FORBIDDEN_APPLICATION_TOKENS = (
    "applications/fsm",
    "applications/happy",
)
ABSOLUTE_PATTERNS = [
    re.compile(r"/media/[A-Za-z0-9_.-]+/"),
    re.compile(r"/Users/[A-Za-z0-9_.-]+/"),
    re.compile(r"[A-Za-z]:/"),
]

def main():
    ap=argparse.ArgumentParser();ap.add_argument("root", nargs="?");args=ap.parse_args()
    root=Path(args.root).resolve() if args.root else Path(__file__).resolve().parents[1]
    problems=[]
    for rel in REQUIRED:
        if not (root/rel).is_file():
            problems.append(f"missing required file: {rel}")

    for p in root.rglob("*"):
        rel=p.relative_to(root)
        if any(part in FORBIDDEN_TREE_PARTS for part in rel.parts):
            problems.append(f"forbidden runtime/development tree: {rel.as_posix()}")
        if p.is_file() and p.suffix.lower() in FORBIDDEN_FILE_SUFFIXES:
            problems.append(f"embedded archive/checksum sidecar not allowed in clean runtime tree: {rel.as_posix()}")

    scan_roots=[root/"framework",root/"tools",root/"docs",root/"templates"]
    for sr in scan_roots:
        if not sr.exists(): continue
        for p in sr.rglob("*"):
            if not p.is_file(): continue
            if p.suffix.lower() not in {".r",".py",".sh",".md",".csv",".txt",".cpp"}:
                continue
            text=p.read_text(encoding="utf-8",errors="replace")
            low=text.lower()
            # Do not flag this checker's own forbidden-token definitions.
            if p.resolve() != Path(__file__).resolve():
                for tok in FORBIDDEN_APPLICATION_TOKENS:
                    if tok in low:
                        problems.append(f"real-application path token {tok!r} in {p.relative_to(root).as_posix()}")
            if p.parts[-2:] and "framework" in p.parts:
                for pat in ABSOLUTE_PATTERNS:
                    if pat.search(text):
                        problems.append(f"machine-specific absolute path in {p.relative_to(root).as_posix()}")

    # Only application template plus applications/README are allowed initially.
    app_root=root/"applications"
    if app_root.exists():
        entries=[p for p in app_root.iterdir() if p.name!="README.md"]
        if entries:
            problems.append("clean baseline applications/ is not empty")

    print("TDABM clean-framework check")
    print(f"Root: {root}")
    print(f"Problems: {len(problems)}")
    for x in problems:
        print("FAIL:",x)
    if not problems:
        print("PASS: required generic modules are present.")
        print("PASS: no real application directories, embedded audit ZIPs, outputs or historical releases are present.")
        print("PASS: generic framework source contains no machine-specific absolute path.")
    return 1 if problems else 0

if __name__=="__main__":
    sys.exit(main())
