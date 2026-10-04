#!/usr/bin/env python3
from pathlib import Path
import argparse,sys
CURRENT_B='P1.4-B v1.1.3'; CURRENT_C='P1.4-C v1.0.3'; STALE_B='P1.4-B v1.1.2'; STALE_C='P1.4-C v1.0.2'
AUTHORITATIVE=['README.md','NEW_CONVERSATION_HANDOVER.md','VERSION','docs/03_RADIUS_POLICY.md','docs/04_TOPOLOGY_FREEZE_AND_RECOLOURING.md','docs/07_ACCEPTED_BASELINE_HISTORY.md','docs/09_RADIUS_POLICY_CONTRACT_CORRECTION.md','APPLICATION_HANDOVER_REQUEST.md']
def main():
 ap=argparse.ArgumentParser(); ap.add_argument('root',nargs='?'); a=ap.parse_args(); root=Path(a.root).resolve() if a.root else Path(__file__).resolve().parents[1]; probs=[]
 vt=(root/'VERSION').read_text(encoding='utf-8',errors='replace')
 for token in ['package_version=1.1.3','radius_diagnostics=P1.4-B v1.1.3','canonical_topology=P1.4-C v1.0.3']:
  if token not in vt: probs.append('VERSION missing '+token)
 for rel in AUTHORITATIVE:
  q=root/rel
  if not q.is_file(): probs.append('missing '+rel); continue
  s=q.read_text(encoding='utf-8',errors='replace')
  if STALE_B in s: probs.append(f'stale {STALE_B} in {rel}')
  if STALE_C in s: probs.append(f'stale {STALE_C} in {rel}')
 for rel in ['README.md','NEW_CONVERSATION_HANDOVER.md']:
  s=(root/rel).read_text(encoding='utf-8',errors='replace')
  if CURRENT_B not in s: probs.append(f'{rel} missing {CURRENT_B}')
  if CURRENT_C not in s: probs.append(f'{rel} missing {CURRENT_C}')
 print('TDABM current-baseline version consistency check'); print('Problems:',len(probs))
 for x in probs: print('FAIL:',x)
 if not probs: print('PASS: authoritative documentation consistently identifies P1.4-B v1.1.3 and P1.4-C v1.0.3.'); print('PASS: package_version is 1.1.3 and no stale current-baseline reference remains.')
 return 1 if probs else 0
if __name__=='__main__': sys.exit(main())
