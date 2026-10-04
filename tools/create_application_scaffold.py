#!/usr/bin/env python3
from pathlib import Path
import argparse, re, shutil, sys

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("application_id")
    ap.add_argument("--root", default=None)
    args = ap.parse_args()
    app = args.application_id.strip()
    if not re.fullmatch(r"[A-Za-z][A-Za-z0-9_-]*", app):
        print("ERROR: application_id must start with a letter and contain only letters, digits, _ or -.")
        return 1
    root = Path(args.root).resolve() if args.root else Path(__file__).resolve().parents[1]
    src = root / "templates" / "new_application"
    dest = root / "applications" / app
    if dest.exists():
        print(f"ERROR: application already exists: {dest}")
        return 1
    shutil.copytree(src, dest)
    print(f"Created application scaffold: {dest}")
    print("Next: recover and complete config/application_contract.csv before analytical execution.")
    return 0

if __name__ == "__main__":
    sys.exit(main())
