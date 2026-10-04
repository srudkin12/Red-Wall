#!/usr/bin/env python3
from __future__ import annotations
import csv, hashlib, math, os, statistics, sys, time, zipfile
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
INP = ROOT / 'input'
OUT = ROOT / 'outputs'
LOGDIR = OUT / 'logs'
OUT.mkdir(exist_ok=True)
LOGDIR.mkdir(exist_ok=True)

EXPECTED = {
    '2021_n_labelled': 41,
    '2021_balls_total': 28,
    '2021_balls_with_label': 11,
    '2021_max_label_share': 1/3,
    '2021_mean_nonlabel_share': 0.89719136344203,
    '2011_balls_with_label': 14,
    '2011_max_label_share': 0.217391304347826,
}
BENCH_ORDER = [
    'unrestricted_41_seat',
    'region_stratified',
    'region_plus_labour_over_conservative_2017',
    'region_plus_2017_labcon_margin_quartile',
]
PRETTY_BENCH = {
    'unrestricted_41_seat': 'unrestricted',
    'region_stratified': 'region',
    'region_plus_labour_over_conservative_2017': 'region + Labour>Conservative 2017',
    'region_plus_2017_labcon_margin_quartile': 'region x 2017 margin quartile',
}
PRETTY_VAR = {
    'population_density':'population density',
    'age_0_15':'age 0-15', 'age_16_24':'age 16-24', 'age_25_34':'age 25-34',
    'age_35_49':'age 35-49', 'age_50_64':'age 50-64', 'age_65_over':'age 65+',
    'ethnic_group_white_british':'White British share',
    'qualifications_none':'no qualifications', 'qualifications_level1':'Level 1 qualifications',
    'qualifications_level2':'Level 2 qualifications', 'qualifications_level3':'Level 3 qualifications',
    'qualifications_level4':'Level 4 qualifications', 'qualifications_other':'other qualifications',
    'NSSEC_higher_managerial_admin_professional':'higher managerial/administrative/professional NS-SEC',
    'NSSEC_lower_managerial_admin_professional':'lower managerial/administrative/professional NS-SEC',
    'NSSEC_intermediate_occupations':'intermediate occupations',
    'NSSEC_small_employers_own_account_workers':'small employers/own-account workers',
    'NSSEC_lower_supervisory_technical':'lower supervisory/technical occupations',
    'NSSEC_semiroutine':'semi-routine occupations', 'NSSEC_routine':'routine occupations',
    'NSSEC_never_worked_long_term_unemployed':'never worked/long-term unemployed',
    'NSSEC_full_time_students':'full-time students',
    'industry_agriculture_energy_water':'agriculture/energy/water employment',
    'industry_manufacturing':'manufacturing employment', 'industry_construction':'construction employment',
    'industry_distribution_hotels_restaurants':'distribution/hotels/restaurants employment',
    'industry_transport_communication':'transport/communication employment',
    'industry_financial_realestate_professional_administrative':'financial/real-estate/professional/administrative employment',
    'industry_public_administration_education_health':'public administration/education/health employment',
    'industry_other':'other-industry employment',
}

def read_csv(path):
    with open(path, newline='', encoding='utf-8-sig') as f:
        return list(csv.DictReader(f))

def write_csv(path, rows, fields=None):
    rows=list(rows)
    if fields is None:
        fields=list(rows[0].keys()) if rows else []
    with open(path,'w',newline='',encoding='utf-8') as f:
        w=csv.DictWriter(f,fieldnames=fields)
        w.writeheader(); w.writerows(rows)

def sha256(path):
    h=hashlib.sha256()
    with open(path,'rb') as f:
        for chunk in iter(lambda:f.read(1024*1024),b''): h.update(chunk)
    return h.hexdigest()

def qtype7(xs,p):
    ys=sorted(xs); n=len(ys)
    if n==1: return ys[0]
    h=(n-1)*p
    lo=math.floor(h); hi=math.ceil(h)
    if lo==hi: return ys[lo]
    return ys[lo]+(h-lo)*(ys[hi]-ys[lo])

def fnum(x):
    try: return float(x)
    except: return math.nan

def pvals(xs, obs):
    n=len(xs)
    lower=(1+sum(x <= obs for x in xs))/(n+1)
    upper=(1+sum(x >= obs for x in xs))/(n+1)
    return lower, upper

def summarize(reps, observed_row, year):
    metrics=['balls_with_label','max_label_share','membership_weighted_local_prevalence','mean_nonlabel_share_among_unique_comembers']
    by=defaultdict(list)
    for r in reps: by[r['benchmark']].append(r)
    out=[]
    for b in BENCH_ORDER:
        rows=by[b]
        if len(rows)!=10000: raise RuntimeError(f'{year} {b}: expected 10000 draws, found {len(rows)}')
        for m in metrics:
            xs=[fnum(r[m]) for r in rows]
            obs=fnum(observed_row[m])
            lo,up=pvals(xs,obs)
            out.append({
                'year':year,'benchmark':b,'benchmark_label':PRETTY_BENCH[b],'metric':m,
                'observed':f'{obs:.15g}','benchmark_median':f'{statistics.median(xs):.15g}',
                'benchmark_q05':f'{qtype7(xs,.05):.15g}','benchmark_q95':f'{qtype7(xs,.95):.15g}',
                'lower_tail_p':f'{lo:.15g}','upper_tail_p':f'{up:.15g}'
            })
    return out

def read_observed_2011():
    r=read_csv(INP/'2011_canonical_redwall_diagnostics.csv')[0]
    required=['balls_with_label','max_label_share','membership_weighted_local_prevalence','mean_nonlabel_share_among_unique_comembers']
    missing=[x for x in required if x not in r]
    if missing:
        raise RuntimeError(f'2011 diagnostic missing {missing}; fields={list(r)}')
    return r

def validation_gate(name, passed, detail):
    return {'gate':name,'status':'PASS' if passed else 'FAIL','detail':detail}

def main():
    log=[]; gates=[]
    # Hash every input.
    hashrows=[]
    for p in sorted(INP.iterdir()):
        if p.is_file(): hashrows.append({'file':p.name,'sha256':sha256(p),'bytes':p.stat().st_size})
    write_csv(OUT/'00_input_hashes.csv',hashrows)

    obs21=read_csv(INP/'2021_canonical_extended_diagnostics.csv')[0]
    reps21=read_csv(INP/'2021_benchmark_replicates.csv')
    obs11=read_observed_2011()
    reps11=read_csv(INP/'2011_benchmark_replicates.csv')

    gates += [
      validation_gate('2021_n_labelled', int(float(obs21['n_labelled']))==EXPECTED['2021_n_labelled'], obs21['n_labelled']),
      validation_gate('2021_balls_total', int(float(obs21['balls_total']))==EXPECTED['2021_balls_total'], obs21['balls_total']),
      validation_gate('2021_balls_with_label', int(float(obs21['balls_with_label']))==EXPECTED['2021_balls_with_label'], obs21['balls_with_label']),
      validation_gate('2021_max_label_share', abs(float(obs21['max_label_share'])-EXPECTED['2021_max_label_share'])<1e-12, obs21['max_label_share']),
      validation_gate('2021_mean_nonlabel_share', abs(float(obs21['mean_nonlabel_share_among_unique_comembers'])-EXPECTED['2021_mean_nonlabel_share'])<1e-12, obs21['mean_nonlabel_share_among_unique_comembers']),
      validation_gate('2011_balls_with_label', int(float(obs11['balls_with_label']))==EXPECTED['2011_balls_with_label'], obs11['balls_with_label']),
      validation_gate('2011_max_label_share', abs(float(obs11['max_label_share'])-EXPECTED['2011_max_label_share'])<1e-12, obs11['max_label_share']),
    ]

    sum21=summarize(reps21,obs21,'2021')
    sum11=summarize(reps11,obs11,'2011')
    write_csv(OUT/'02_prevalence_mixing_benchmark_summary_2021.csv',sum21)
    write_csv(OUT/'03_prevalence_mixing_benchmark_summary_2011.csv',sum11)

    # Manuscript-facing subset.
    mf=[]
    for rows in [sum21,sum11]:
        for r in rows:
            if r['metric'] in ('max_label_share','mean_nonlabel_share_among_unique_comembers','balls_with_label'):
                mf.append(r)
    write_csv(OUT/'04_manuscript_facing_calibration.csv',mf)

    # Build canonical membership/profile tables.
    axes=read_csv(INP/'2021_canonical_scaled_axes.csv')
    axes_by_id={r['unit_id']:r for r in axes}
    rwaudit=read_csv(INP/'2021_redwall_source_match_audit.csv')
    rw_ids={r['matched_ons_id'] for r in rwaudit if r.get('matched','').upper()=='TRUE'}
    memberships=read_csv(INP/'2021_canonical_memberships.csv')
    members=defaultdict(list)
    for r in memberships: members[int(r['ball_id'])].append(r['unit_id'])
    bdiag={int(r['ball_id']):r for r in read_csv(INP/'2021_canonical_ball_diagnostics.csv')}
    comps={int(r['ball_id']):r for r in read_csv(INP/'2021_canonical_ball_components.csv')}
    edges=read_csv(INP/'2021_canonical_edge_diagnostics.csv')

    occupied=[int(b) for b,r in bdiag.items() if int(float(r['labelled']))>0]
    occupied_sorted=sorted(occupied,key=lambda b:float(bdiag[b]['labelled_share']),reverse=True)
    top=occupied_sorted[:3]
    profile_rows=[]; axis_rows=[]
    varcols=[c for c in axes[0] if c not in ('point_index','unit_id','unit_label')]
    for b in top:
        ids=members[b]
        means={v:sum(float(axes_by_id[i][v]) for i in ids)/len(ids) for v in varcols}
        ranked=sorted(means.items(), key=lambda kv:abs(kv[1]), reverse=True)
        d=bdiag[b]
        profile_rows.append({
          'ball_id':b,'component_id':comps[b]['component_id'],'degree':comps[b].get('degree',''),
          'ball_size':d['ball_size'],'redwall_count':d['labelled'],'redwall_share':d['labelled_share'],
          'top_axis_1':PRETTY_VAR.get(ranked[0][0],ranked[0][0]),'top_axis_1_mean_z':f'{ranked[0][1]:.6f}',
          'top_axis_2':PRETTY_VAR.get(ranked[1][0],ranked[1][0]),'top_axis_2_mean_z':f'{ranked[1][1]:.6f}',
          'top_axis_3':PRETTY_VAR.get(ranked[2][0],ranked[2][0]),'top_axis_3_mean_z':f'{ranked[2][1]:.6f}',
          'top_axis_4':PRETTY_VAR.get(ranked[3][0],ranked[3][0]),'top_axis_4_mean_z':f'{ranked[3][1]:.6f}',
          'top_axis_5':PRETTY_VAR.get(ranked[4][0],ranked[4][0]),'top_axis_5_mean_z':f'{ranked[4][1]:.6f}',
          'top_axis_6':PRETTY_VAR.get(ranked[5][0],ranked[5][0]),'top_axis_6_mean_z':f'{ranked[5][1]:.6f}',
        })
        for rank,(v,val) in enumerate(ranked[:10],1):
            axis_rows.append({'ball_id':b,'rank_abs_mean_z':rank,'variable':v,'label':PRETTY_VAR.get(v,v),'mean_z':f'{val:.9f}','direction':'above sample mean' if val>0 else 'below sample mean'})
    write_csv(OUT/'05_high_prevalence_ball_profiles.csv',profile_rows)
    write_csv(OUT/'06_high_prevalence_ball_top_axes.csv',axis_rows)

    # Full highest-prevalence ball membership, with label status and overlap with Ball 11 if applicable.
    bmax=top[0]
    b11=set(members.get(11,[]))
    mix=[]
    for uid in sorted(members[bmax], key=lambda u: axes_by_id[u]['unit_label']):
        mix.append({'ball_id':bmax,'unit_id':uid,'constituency':axes_by_id[uid]['unit_label'],'redwall':uid in rw_ids,'also_member_ball_11':uid in b11})
    write_csv(OUT/'07_highest_prevalence_ball_mixed_membership.csv',mix)

    # Neighbours of top balls.
    nbr=[]
    for b in top:
        for e in edges:
            f=int(e['from']); t=int(e['to'])
            if b not in (f,t): continue
            n=t if f==b else f
            nbr.append({'ball_id':b,'neighbor_ball_id':n,'overlap_strength':e['strength'],
                        'neighbor_ball_size':bdiag[n]['ball_size'],'neighbor_redwall_count':bdiag[n]['labelled'],
                        'neighbor_redwall_share':bdiag[n]['labelled_share']})
    write_csv(OUT/'08_high_prevalence_ball_neighbors.csv',nbr)

    # Audit accepted domain-balanced sensitivity.
    bw=read_csv(INP/'accepted_block_weighting_redwall_summary.csv')[0]
    bwdef=read_csv(INP/'accepted_block_weighting_definition.csv')
    accepted=(INP/'accepted_block_weighting_ACCEPTANCE.txt').read_text(encoding='utf-8')
    bw_pass=('BLOCK_WEIGHTING_PASS: TRUE' in accepted or 'block_majority_frequency=0' in accepted) and abs(float(bw['majority_redwall_ball_frequency']))<1e-15
    gates.append(validation_gate('accepted_equal_domain_block_weighting_no_majority',bw_pass,f"majority frequency={bw['majority_redwall_ball_frequency']}; radius={bw['weighted_radius']}"))
    write_csv(OUT/'09_domain_balanced_sensitivity_audit.csv',[{
      'accepted_status':'PASS' if bw_pass else 'FAIL','weighted_radius':bw['weighted_radius'],
      'distance_scale_ratio':bw['distance_scale_ratio'],'majority_redwall_ball_frequency':bw['majority_redwall_ball_frequency'],
      'median_max_redwall_share':bw['max_redwall_share_median'],'q10_max_redwall_share':bw['max_redwall_share_q10'],
      'q90_max_redwall_share':bw['max_redwall_share_q90'],'n_domains':len(bwdef),
      'weighting_rule':'each domain given equal total squared-distance weight via 1/sqrt(number of coordinates)'
    }])

    write_csv(OUT/'01_validation_gates.csv',gates)
    failed=[g for g in gates if g['status']!='PASS']

    def find(rows,year,b,m):
        return next(r for r in rows if r['year']==year and r['benchmark']==b and r['metric']==m)
    # Manuscript-facing Markdown.
    lines=[]
    lines += ['# v1.6 empirical response summary','',
      '## Size-aware 2021 prevalence and mixing calibration','',
      f"Observed maximum within-ball Red Wall prevalence is {float(obs21['max_label_share'])*100:.1f}%. Its upper-tail p-values across the accepted benchmark sequence are " +
      ', '.join(f"{PRETTY_BENCH[b]} {float(find(sum21,'2021',b,'max_label_share')['upper_tail_p']):.3f}" for b in BENCH_ORDER) + '.',
      f"Observed mean non-Red-Wall share among each Red Wall seat's unique co-members is {float(obs21['mean_nonlabel_share_among_unique_comembers'])*100:.1f}%. Lower-tail p-values (testing unusually low non-Red-Wall mixing / stronger local assortativity) are " +
      ', '.join(f"{PRETTY_BENCH[b]} {float(find(sum21,'2021',b,'mean_nonlabel_share_among_unique_comembers')['lower_tail_p']):.3f}" for b in BENCH_ORDER) + '.',
      '', 'Interpretation: the Red Wall is unusually concentrated under broad benchmarks, but its maximum local prevalence is not unusually high under any accepted benchmark. Reduced mixing is visible against unrestricted and region-only labels, but not once prior electoral structure is preserved.', '',
      '## High-prevalence canonical neighbourhoods','']
    for r in profile_rows:
        lines.append(f"- Ball {r['ball_id']}: {int(float(r['redwall_count']))}/{int(float(r['ball_size']))} Red Wall ({float(r['redwall_share'])*100:.1f}%). Largest standardized profile deviations include {r['top_axis_1']} ({float(r['top_axis_1_mean_z']):+.2f}), {r['top_axis_2']} ({float(r['top_axis_2_mean_z']):+.2f}), {r['top_axis_3']} ({float(r['top_axis_3_mean_z']):+.2f}), and {r['top_axis_4']} ({float(r['top_axis_4_mean_z']):+.2f}).")
    lines += ['', f"Ball {bmax}, the highest-prevalence neighbourhood, contains {sum(1 for r in mix if r['redwall'])} Red Wall and {sum(1 for r in mix if not r['redwall'])} non-Red-Wall constituencies. It is connected only to Ball 11, with five shared constituencies; both Red Wall members of Ball {bmax} also belong to Ball 11.", '',
      '## Equal-domain weighting audit','',
      f"The accepted block-weighted sensitivity gives equal total squared-distance weight to each substantive domain. Across 1,000 final-order constructions its majority-Red-Wall-ball frequency is {float(bw['majority_redwall_ball_frequency']):.0f}; median maximum Red Wall share is {float(bw['max_redwall_share_median'])*100:.1f}%. This package audits, rather than reruns, that accepted sensitivity.", '',
      '## Status','', 'PASS' if not failed else 'FAIL']
    (OUT/'V1_6_EMPIRICAL_SUMMARY.md').write_text('\n'.join(lines)+'\n',encoding='utf-8')

    status='PASS' if not failed else 'FAIL'
    (OUT/'RUN_STATUS.txt').write_text(f'OTWAY_BJPOLS_V1_6_EVIDENCE={status}\nfailed_gates={len(failed)}\n',encoding='utf-8')
    logfile=LOGDIR/f"run_{time.strftime('%Y%m%d_%H%M%S')}.log"
    logfile.write_text('\n'.join([f"status={status}",f"failed_gates={len(failed)}",f"2021_replicates={len(reps21)}",f"2011_replicates={len(reps11)}"])+"\n",encoding='utf-8')
    if failed:
        for g in failed: print('FAIL',g,file=sys.stderr)
        return 2
    print((OUT/'V1_6_EMPIRICAL_SUMMARY.md').read_text(encoding='utf-8'))
    return 0

if __name__=='__main__':
    raise SystemExit(main())
