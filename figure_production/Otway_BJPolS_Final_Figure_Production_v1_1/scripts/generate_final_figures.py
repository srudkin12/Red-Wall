#!/usr/bin/env python3
from pathlib import Path
import os, math, hashlib
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize, TwoSlopeNorm

PACKAGE_DIR = Path(__file__).resolve().parents[1]
REF = PACKAGE_DIR / "reference"
OUT = PACKAGE_DIR / "outputs"
OUT.mkdir(parents=True, exist_ok=True)

EXPECTED_FROZEN_SHA256 = "543c52e6090af09b51f5617d08ea722d12a6303371cd10caa9538b96ada70f18"
EXPECTED_TOPOLOGY_FINGERPRINT = "2a7fae54601a98262d7196c0f3ad8c56d43bde998638d95ca24adfffdba4c539"

def fail(msg):
    raise RuntimeError(msg)

def sha256(path):
    h=hashlib.sha256()
    with open(path,"rb") as f:
        for chunk in iter(lambda:f.read(1024*1024),b""):
            h.update(chunk)
    return h.hexdigest()

def find_repo_root():
    env=os.environ.get("OTWAY_ROOT","").strip()
    candidates=[]
    if env:
        candidates.append(Path(env))
    candidates += [
        PACKAGE_DIR.parents[1],
        PACKAGE_DIR.parent,
    ]
    p=PACKAGE_DIR.resolve()
    for _ in range(10):
        candidates.append(p)
        if p.parent==p:
            break
        p=p.parent
    for c in candidates:
        if (c/"applications"/"Otway_Rudkin_2024_Corrected31D").is_dir():
            return c.resolve()
    fail("Could not discover TDABM_Portability root. Set OTWAY_ROOT if needed.")

ROOT=find_repo_root()
FROZEN=ROOT/"applications"/"Otway_Rudkin_2024_Corrected31D"/"frozen_input"/"p1_4_a_v1_0"/"Otway_Rudkin_2024_Corrected31D_input_frozen_P1_4_A_v1_0.csv"
if not FROZEN.exists():
    fail(f"Missing frozen input: {FROZEN}")
observed_frozen_sha=sha256(FROZEN).lower()
source_mode=os.environ.get("OTWAY_SOURCE_MODE","strict").strip().lower()
if observed_frozen_sha!=EXPECTED_FROZEN_SHA256.lower() and source_mode!="reconstructed":
    fail("Frozen 2021 input SHA-256 mismatch. Set OTWAY_SOURCE_MODE=reconstructed only for a documented user-reconstructed source.")
if observed_frozen_sha!=EXPECTED_FROZEN_SHA256.lower() and source_mode=="reconstructed":
    print("WARNING: figure generation uses a reconstructed source; canonical topology fingerprint remains binding.")

dat=pd.read_csv(FROZEN,dtype={"ONSConstID":str})
members=pd.read_csv(REF/"canonical_memberships.csv",dtype={"unit_id":str})
edges=pd.read_csv(REF/"canonical_overlap_edges.csv")
ball_meta=pd.read_csv(REF/"canonical_ball_components.csv")
topology=pd.read_csv(REF/"canonical_topology_summary.csv")
rw_audit=pd.read_csv(REF/"redwall_source_match_audit.csv",dtype={"matched_ons_id":str})
layout=pd.read_csv(REF/"canonical_layout_from_current_figure2_v1_0.csv")

if len(dat)!=571 or dat["ONSConstID"].nunique()!=571:
    fail("Frozen data must contain 571 unique ONS constituency IDs.")
if len(ball_meta)!=28 or len(edges)!=96:
    fail("Canonical topology must contain 28 balls and 96 overlap edges.")
if int(topology.loc[0,"n_components"])!=2 or int(topology.loc[0,"n_isolated_balls"])!=1:
    fail("Canonical topology component contract failed.")
if str(topology.loc[0,"topology_fingerprint_sha256"])!=EXPECTED_TOPOLOGY_FINGERPRINT:
    fail("Canonical topology fingerprint mismatch.")
if sorted(layout.ball_id.tolist())!=list(range(1,29)):
    fail("Fixed layout must contain balls 1..28 exactly once.")
if not set(members.unit_id).issubset(set(dat.ONSConstID)):
    missing=sorted(set(members.unit_id)-set(dat.ONSConstID))
    fail(f"Canonical membership IDs missing from frozen input: {missing[:5]}")

required=["ONSConstID","Con15","Con17","Con19","left_behind_index_corrected"]
missing=[c for c in required if c not in dat.columns]
if missing:
    fail("Frozen input is missing required figure columns: "+", ".join(missing))

for c in ["Con15","Con17","Con19","left_behind_index_corrected"]:
    dat[c]=pd.to_numeric(dat[c],errors="coerce")
    if not np.isfinite(dat[c]).all():
        fail(f"Non-finite values in {c}.")

flag=rw_audit["matched"].astype(str).str.strip().str.lower().isin(["true","t","1","yes","y"])
rw_ids=rw_audit.loc[flag,"matched_ons_id"].dropna().astype(str).str.strip()
if len(rw_audit)!=42 or rw_ids.nunique()!=41:
    fail("Red Wall source audit must contain 42 source seats and 41 unique matched IDs.")
redwall=set(rw_ids.tolist())
if not redwall.issubset(set(dat.ONSConstID)):
    fail("Not all 41 Red Wall IDs are present in frozen 2021 input.")

dat_idx=dat.set_index("ONSConstID",drop=False)

rows=[]
for b in range(1,29):
    ids=members.loc[members.ball_id==b,"unit_id"].astype(str).tolist()
    if len(ids)==0:
        fail(f"Ball {b} has no members.")
    d=dat_idx.loc[ids]
    n_rw=sum(i in redwall for i in ids)
    rows.append({
        "ball_id":b,
        "ball_size":len(ids),
        "component_id":int(ball_meta.loc[ball_meta.ball_id==b,"component_id"].iloc[0]),
        "degree":int(ball_meta.loc[ball_meta.ball_id==b,"degree"].iloc[0]),
        "left_behind_mean":float(d["left_behind_index_corrected"].mean()),
        "redwall_n":int(n_rw),
        "redwall_share":float(n_rw/len(ids)),
        "Con15_mean":float(d["Con15"].mean()),
        "Con17_mean":float(d["Con17"].mean()),
        "Con19_mean":float(d["Con19"].mean()),
        "Con1517_change_mean":float((d["Con17"]-d["Con15"]).mean()),
        "Con1719_change_mean":float((d["Con19"]-d["Con17"]).mean()),
    })
ball=pd.DataFrame(rows).merge(layout[["ball_id","x_px","y_px"]],on="ball_id",how="left",validate="one_to_one")
ball["x"]=ball["x_px"]
ball["y"]=1150-ball["y_px"]

gates=[]
def gate(name,passed,detail):
    gates.append({"gate":name,"status":"PASS" if passed else "FAIL","detail":detail})
    if not passed:
        pd.DataFrame(gates).to_csv(OUT/"00_figure_generation_gates.csv",index=False)
        fail(f"{name}: {detail}")

gate("frozen_input_sha256",True,sha256(FROZEN))
gate("canonical_28_balls",len(ball)==28,f"balls={len(ball)}")
gate("canonical_96_edges",len(edges)==96,f"edges={len(edges)}")
gate("redwall_41_matched",len(redwall)==41,f"n={len(redwall)}")
gate("leftbehind_unit_interval",
     ball.left_behind_mean.min()>=-1e-12 and ball.left_behind_mean.max()<=1+1e-12,
     f"range={ball.left_behind_mean.min():.6f}..{ball.left_behind_mean.max():.6f}")
gate("redwall_share_unit_interval",
     ball.redwall_share.min()>=-1e-12 and ball.redwall_share.max()<=1+1e-12,
     f"range={ball.redwall_share.min():.6f}..{ball.redwall_share.max():.6f}")
gate("redwall_max_share_approx_one_third",
     abs(ball.redwall_share.max()-(1/3))<0.002,
     f"max={ball.redwall_share.max():.6f}")
gate("no_majority_redwall_ball",
     bool((ball.redwall_share<=0.5+1e-12).all()),
     f"max={ball.redwall_share.max():.6f}")

r1517=(ball.Con1517_change_mean.min(),ball.Con1517_change_mean.max())
r1719=(ball.Con1719_change_mean.min(),ball.Con1719_change_mean.max())
gate("figure4_2015_2017_range_matches_manuscript",
     round(r1517[0],1)==-7.5 and round(r1517[1],1)==13.3,
     f"rounded range={round(r1517[0],1)}..{round(r1517[1],1)}")
gate("figure4_2017_2019_range_matches_manuscript",
     round(r1719[0],1)==-4.6 and round(r1719[1],1)==8.6,
     f"rounded range={round(r1719[0],1)}..{round(r1719[1],1)}")

top10=ball.nlargest(10,"left_behind_mean")
gate("top10_leftbehind_positive_change_both_intervals",
     bool((top10.Con1517_change_mean>0).all() and (top10.Con1719_change_mean>0).all()),
     "all ten highest left-behind balls have positive Conservative change in both intervals")

vote_min=min(ball.Con15_mean.min(),ball.Con17_mean.min())
vote_max=max(ball.Con15_mean.max(),ball.Con17_mean.max())
gate("figure4_vote_scale_10_60_no_clipping",
     vote_min>=10-1e-9 and vote_max<=60+1e-9,
     f"combined range={vote_min:.4f}..{vote_max:.4f}")

pd.DataFrame(gates).to_csv(OUT/"00_figure_generation_gates.csv",index=False)
ball.to_csv(OUT/"01_canonical_ball_figure_data.csv",index=False)

pos={int(r.ball_id):(float(r.x),float(r.y)) for _,r in ball.iterrows()}
size_map={int(r.ball_id):250.0+8.5*float(r.ball_size) for _,r in ball.iterrows()}

def draw_graph(ax, values, norm, cmap, title, cbar_ticks=None, cbar_ticklabels=None, majority_line=False):
    for _,e in edges.iterrows():
        a,b=int(e["from"]),int(e["to"])
        xa,ya=pos[a]; xb,yb=pos[b]
        ax.plot([xa,xb],[ya,yb],color="0.68",alpha=.30,lw=1.05,zorder=1)
    xs=[pos[b][0] for b in range(1,29)]
    ys=[pos[b][1] for b in range(1,29)]
    ss=[size_map[b] for b in range(1,29)]
    vv=np.asarray([values[b] for b in range(1,29)],dtype=float)
    sc=ax.scatter(xs,ys,s=ss,c=vv,cmap=cmap,norm=norm,
                  edgecolors="black",linewidths=.85,zorder=2)
    for b in range(1,29):
        ax.text(pos[b][0],pos[b][1],str(b),ha="center",va="center",
                fontsize=8.4,color="black",zorder=3)
    ax.set_title(title,fontsize=15,pad=10)
    ax.set_xlim(45,1085); ax.set_ylim(15,1030)
    ax.set_aspect("equal",adjustable="box"); ax.axis("off")
    cbar=plt.colorbar(sc,ax=ax,fraction=0.037,pad=0.025,ticks=cbar_ticks)
    cbar.ax.tick_params(labelsize=9)
    if cbar_ticklabels is not None:
        cbar.ax.set_yticklabels(cbar_ticklabels)
    if majority_line:
        cbar.ax.axhline(0.5,color="black",lw=1.2,ls="--")
    return sc

# Figure 3
fig,axs=plt.subplots(1,2,figsize=(12.5,5.95),dpi=200)
draw_graph(axs[0],dict(zip(ball.ball_id,ball.left_behind_mean)),
           Normalize(0,1),"viridis","Conventional left-behind index",
           cbar_ticks=np.arange(0,1.01,.2),
           cbar_ticklabels=[f"{x:.1f}" for x in np.arange(0,1.01,.2)])
ticks3=np.arange(0,1.01,.25)
draw_graph(axs[1],dict(zip(ball.ball_id,ball.redwall_share)),
           Normalize(0,1),"viridis","Red Wall share within each ball",
           cbar_ticks=ticks3,
           cbar_ticklabels=[f"{x:.2f}" for x in ticks3],majority_line=True)
fig.subplots_adjust(left=.02,right=.965,top=.91,bottom=.035,wspace=.18)
for ext in ["png","pdf","svg"]:
    fig.savefig(OUT/f"Figure3_LeftBehindedness_RedWall_Refined.{ext}",dpi=200,bbox_inches="tight",pad_inches=0.08)
plt.close(fig)

# Figure 4
fig,axs=plt.subplots(2,2,figsize=(12.5,11.0),dpi=200)
vote_norm=Normalize(10,60)
vote_ticks=np.arange(10,61,10)
draw_graph(axs[0,0],dict(zip(ball.ball_id,ball.Con15_mean)),
           vote_norm,"viridis","Conservative vote, 2015 (%)",
           cbar_ticks=vote_ticks,cbar_ticklabels=[str(int(x)) for x in vote_ticks])
draw_graph(axs[0,1],dict(zip(ball.ball_id,ball.Con17_mean)),
           vote_norm,"viridis","Conservative vote, 2017 (%)",
           cbar_ticks=vote_ticks,cbar_ticklabels=[str(int(x)) for x in vote_ticks])

max_abs=max(abs(ball.Con1517_change_mean).max(),abs(ball.Con1719_change_mean).max())
shared_lim=float(math.ceil(max_abs))
change_norm=TwoSlopeNorm(vmin=-shared_lim,vcenter=0,vmax=shared_lim)
ticks=np.arange(-shared_lim,shared_lim+0.001,2.0)
draw_graph(axs[1,0],dict(zip(ball.ball_id,ball.Con1517_change_mean)),
           change_norm,"coolwarm","Change, 2015-2017 (pp)",cbar_ticks=ticks)
draw_graph(axs[1,1],dict(zip(ball.ball_id,ball.Con1719_change_mean)),
           change_norm,"coolwarm","Change, 2017-2019 (pp)",cbar_ticks=ticks)
fig.subplots_adjust(left=.018,right=.965,top=.955,bottom=.025,wspace=.17,hspace=.08)
for ext in ["png","pdf","svg"]:
    fig.savefig(OUT/f"Figure4_ConservativeSupport_Change_Refined.{ext}",dpi=200,bbox_inches="tight",pad_inches=0.08)
plt.close(fig)

ball[["ball_id","ball_size","x_px","y_px","left_behind_mean","redwall_n","redwall_share"]].to_csv(
    OUT/"02_Figure3_data.csv",index=False)
ball[["ball_id","ball_size","x_px","y_px","Con15_mean","Con17_mean","Con19_mean",
      "Con1517_change_mean","Con1719_change_mean"]].to_csv(
    OUT/"03_Figure4_data.csv",index=False)

pd.DataFrame([
    {"figure":"3 left","measure":"left-behind index","vmin":0,"vmax":1,
     "reference":"","rule":"fixed substantive unit interval"},
    {"figure":"3 right","measure":"Red Wall share","vmin":0,"vmax":1,
     "reference":"0.5 majority threshold","rule":"full prevalence scale"},
    {"figure":"4 top pair","measure":"Conservative vote share","vmin":10,"vmax":60,
     "reference":"","rule":"shared scale retained from current manuscript"},
    {"figure":"4 bottom pair","measure":"Conservative vote-share change",
     "vmin":-shared_lim,"vmax":shared_lim,"reference":"0",
     "rule":"one common symmetric scale across both change panels"},
]).to_csv(OUT/"04_colour_scale_contract.csv",index=False)

summary=f"""# Final figure production summary

Frozen input SHA-256: `{sha256(FROZEN)}`

Canonical topology: 28 balls, 96 edges, 2 components, 1 isolate.

## Figure 3
- Red Wall prevalence scale: 0 to 1.
- Visible majority reference: 0.5.
- Observed maximum Red Wall share: {ball.redwall_share.max():.4f}.

## Figure 4
- 2015 and 2017 vote panels: common 10 to 60 scale.
- Both change panels: common symmetric scale {-shared_lim:.0f} to +{shared_lim:.0f} pp.
- Observed 2015-2017 range: {r1517[0]:.4f} to {r1517[1]:.4f}.
- Observed 2017-2019 range: {r1719[0]:.4f} to {r1719[1]:.4f}.

## Layout
Ball coordinates are fixed from current manuscript Figure 2; no stochastic
layout algorithm is run.
"""
(OUT/"FIGURE_PRODUCTION_SUMMARY.md").write_text(summary,encoding="utf-8")

pd.DataFrame([
    {"field":"repo_root","value":str(ROOT)},
    {"field":"frozen_input","value":str(FROZEN)},
    {"field":"frozen_input_sha256","value":sha256(FROZEN)},
    {"field":"topology_fingerprint","value":EXPECTED_TOPOLOGY_FINGERPRINT},
    {"field":"layout_contract","value":"fixed coordinates from v1.3 embedded Figure 2"},
    {"field":"figure3_redwall_scale","value":"0..1 with 0.5 majority reference"},
    {"field":"figure4_change_scale","value":f"{-shared_lim:.0f}..{shared_lim:.0f}, common symmetric"},
]).to_csv(OUT/"05_figure_provenance.csv",index=False)

(OUT/"RUN_STATUS.txt").write_text(
    "status=PASS\nstage=FINAL_FIGURE_PRODUCTION\n"
    "figure3=PASS\nfigure4=PASS\n"
    f"figure4_shared_change_limit={shared_lim:.0f}\n",
    encoding="utf-8")
print("PASS: final Figures 3 and 4 generated.")
print(f"Figure 4 common symmetric change scale: {-shared_lim:.0f} to +{shared_lim:.0f} pp")
