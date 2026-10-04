# Otway-Rudkin BJPolS final figure production v1.1

v1.1 is the **authoritative final figure source package**.

The v1.0 computation passed every empirical gate, but visual QA of the
handback found that the right-hand colour-bar tick labels were clipped at the
export boundary in Figures 3 and 4. v1.1 changes only figure export/layout
code:

- explicit colour-bar tick labels;
- `bbox_inches="tight"` with padding;
- slightly larger right-hand plot margin;
- no change to empirical values, topology, colour scales or manuscript text.

The input manuscript is v1.4 from the successful v1.0 handback. The output is:

`outputs/Otway_Rudkin_Shape_of_Left_Behindedness_BJPolS_v1_5_Final_Figures_QA.docx`

Run from the standard project root:

```bash
cd /path/to/otway-rudkin-red-wall-ball-mapper
unzip -o Otway_BJPolS_Final_Figure_Production_v1_1.zip
cd Otway_BJPolS_Final_Figure_Production_v1_1
chmod +x run_all.sh
./run_all.sh
```

Expected substantive contracts remain:

- Figure 3 Red Wall prevalence scale: 0-1 with 0.5 majority reference;
- Figure 4 vote panels: common 10-60 scale;
- Figure 4 change panels: common symmetric -14 to +14 pp scale;
- fixed Figure 2 node layout;
- frozen 2021 input SHA and P1.4-C topology fingerprint unchanged.
