#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
REMOTE_SSH="git@github.com:srudkin12/Red-Wall.git"
REMOTE_HTTPS="https://github.com/srudkin12/Red-Wall"

cd "$ROOT"

echo "===== PREPUBLICATION CHECKS ====="
python3 scripts/static_repo_audit.py
bash scripts/prepublication_check.sh

echo
echo "===== SSH / REMOTE CHECK ====="
echo "Target: $REMOTE_HTTPS"
ssh -T git@github.com || true

if [ ! -d .git ]; then
  git init
fi

# Use/rename the current branch as main.
current_branch="$(git symbolic-ref --short HEAD 2>/dev/null || true)"
if [ -n "$current_branch" ] && [ "$current_branch" != "main" ]; then
  git branch -M main
elif [ -z "$current_branch" ]; then
  git checkout -b main
fi

if git remote get-url origin >/dev/null 2>&1; then
  current_remote="$(git remote get-url origin)"
  if [ "$current_remote" != "$REMOTE_SSH" ]; then
    echo "Updating origin from: $current_remote"
    git remote set-url origin "$REMOTE_SSH"
  fi
else
  git remote add origin "$REMOTE_SSH"
fi

# Never force-overwrite an existing remote history.
remote_heads="$(git ls-remote --heads origin 2>/dev/null || true)"
if [ -n "$remote_heads" ]; then
  echo "ERROR: remote repository already contains branch history."
  echo "Refusing an automatic overwrite."
  echo "Remote heads:"
  echo "$remote_heads"
  echo
  echo "Reconcile the existing remote manually, then rerun."
  exit 20
fi

echo
echo "===== STAGING CHECK ====="
git add .

if git status --short | grep -E '(^|/)(merged_data\.csv|redwall_source\.csv)|frozen_input|frozen_topology|raw_nomis|outputs/' ; then
  echo "ERROR: empirical/generated files would be committed." >&2
  git status --short
  exit 21
fi

echo "Files staged for initial publication:"
git status --short

if git diff --cached --quiet; then
  if git rev-parse --verify HEAD >/dev/null 2>&1; then
    echo "No changes to commit."
  else
    echo "ERROR: no files staged for the initial commit." >&2
    exit 22
  fi
else
  git commit -m "Initial public replication repository"
fi

echo
echo "===== PUSH ====="
git push -u origin main

echo
echo "Published:"
echo "$REMOTE_HTTPS"
