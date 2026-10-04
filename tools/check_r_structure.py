#!/usr/bin/env python3
"""Lightweight lexical balance check for R files when R is not available."""
from pathlib import Path
import sys

root = Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else Path(__file__).resolve().parents[1]
errors=[]
for path in root.rglob('*.R'):
    if 'provenance' in path.parts:
        continue
    text=path.read_text(errors='ignore')
    stack=[]
    in_single=in_double=in_backtick=False
    escape=False
    comment=False
    pairs={')':'(',']':'[','}':'{'}
    opens=set(pairs.values())
    line=1
    for ch in text:
        if ch=='\n':
            line+=1; comment=False
            continue
        if comment: continue
        if escape:
            escape=False; continue
        if ch=='\\' and (in_single or in_double):
            escape=True; continue
        if not in_double and not in_backtick and ch=="'":
            in_single=not in_single; continue
        if not in_single and not in_backtick and ch=='"':
            in_double=not in_double; continue
        if not in_single and not in_double and ch=='`':
            in_backtick=not in_backtick; continue
        if not in_single and not in_double and not in_backtick and ch=='#':
            comment=True; continue
        if in_single or in_double or in_backtick: continue
        if ch in opens: stack.append((ch,line))
        elif ch in pairs:
            if not stack or stack[-1][0]!=pairs[ch]:
                errors.append(f'{path.relative_to(root)}:{line}: unmatched {ch}')
                break
            stack.pop()
    if in_single or in_double or in_backtick:
        errors.append(f'{path.relative_to(root)}: unterminated string')
    if stack:
        ch,ln=stack[-1]
        errors.append(f'{path.relative_to(root)}:{ln}: unmatched {ch}')
print('TDABM P1.3.4 lexical R structure check')
for e in errors: print('ERROR:',e)
print(f'Errors: {len(errors)}')
sys.exit(1 if errors else 0)
