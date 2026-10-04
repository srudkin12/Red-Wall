#!/usr/bin/env python3
from pathlib import Path
from zipfile import ZipFile, ZIP_DEFLATED
import hashlib

PKG=Path(__file__).resolve().parents[1]
IN=PKG/"input"/"Otway_Rudkin_Shape_of_Left_Behindedness_BJPolS_v1_4_Final_Figures.docx"
OUTDIR=PKG/"outputs"
OUT=OUTDIR/"Otway_Rudkin_Shape_of_Left_Behindedness_BJPolS_v1_5_Final_Figures_QA.docx"
FIG3=OUTDIR/"Figure3_LeftBehindedness_RedWall_Refined.png"
FIG4=OUTDIR/"Figure4_ConservativeSupport_Change_Refined.png"

EXPECTED_DOCX_SHA="2b1e500897430556123872549ad5ab4d95c250a279b4b7364a912c04c8a7785a"
EXPECTED_OLD_FIG3_SHA="62cf5a34e5c2b8d906940e0e1dbad6471af6847e43e7148c94ec507ec77cb4c6"
EXPECTED_OLD_FIG4_SHA="984bd32eaab0eccdf200dff0e99f7be23eea1321a727c9e094d8acd986eea54f"

def sha_bytes(b): return hashlib.sha256(b).hexdigest()
def sha_file(p): return hashlib.sha256(Path(p).read_bytes()).hexdigest()

if sha_file(IN)!=EXPECTED_DOCX_SHA:
    raise RuntimeError("Input v1.4 manuscript SHA-256 mismatch.")
if not FIG3.exists() or not FIG4.exists():
    raise RuntimeError("Refined figures do not exist; run generation first.")

with ZipFile(IN,"r") as zin:
    f3old=zin.read("word/media/image36.png")
    f4old=zin.read("word/media/image37.png")
    if sha_bytes(f3old)!=EXPECTED_OLD_FIG3_SHA:
        raise RuntimeError("Embedded Figure 3 reference image hash mismatch.")
    if sha_bytes(f4old)!=EXPECTED_OLD_FIG4_SHA:
        raise RuntimeError("Embedded Figure 4 reference image hash mismatch.")
    with ZipFile(OUT,"w",ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            if item.filename=="word/media/image36.png":
                zout.writestr(item,FIG3.read_bytes())
            elif item.filename=="word/media/image37.png":
                zout.writestr(item,FIG4.read_bytes())
            else:
                zout.writestr(item,zin.read(item.filename))

with ZipFile(OUT,"r") as z:
    if sha_bytes(z.read("word/media/image36.png"))!=sha_file(FIG3):
        raise RuntimeError("Patched Figure 3 hash mismatch.")
    if sha_bytes(z.read("word/media/image37.png"))!=sha_file(FIG4):
        raise RuntimeError("Patched Figure 4 hash mismatch.")

(OUTDIR/"MANUSCRIPT_PATCH_STATUS.txt").write_text(
    "status=PASS\n"
    "input=v1.4 Final Figures\n"
    "output=v1.5 Final Figures QA\n"
    "Figure3_media=word/media/image36.png\n"
    "Figure4_media=word/media/image37.png\n"
    f"output_sha256={sha_file(OUT)}\n",
    encoding="utf-8")
print("PASS: refined Figures 3 and 4 patched into v1.4 manuscript.")
print(OUT)
