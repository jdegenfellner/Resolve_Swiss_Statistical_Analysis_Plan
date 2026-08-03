# RESOLVE Swiss — statistical analysis plan (manuscript)

Target journal: **Brazilian Journal of Physical Therapy**, article type *Clinical Trial
Protocol* — the same slot in the same journal where the Australian RESOLVE SAP appeared
(Bagg et al., Braz J Phys Ther 2021;25(1):103–111, doi 10.1016/j.bjpt.2020.06.002).

## Files

| File | What it is |
|---|---|
| `paper.tex` | the manuscript (LaTeX master) |
| `paper.pdf` | current build, 23 pages |
| `references.bib` | 45 entries, every DOI verified against Crossref on 2026-08-03 |
| `R/sample_size.R` | produces every number in the *Sample size* section; base R only |

Build with `pdflatex → bibtex → pdflatex → pdflatex` (plain `latexmk` trips over a stale
`.aux` if you delete build artefacts by hand — delete them all or none).

Open items that need input from the trial team are marked `\open{...}` in the source and
render in **red** in the PDF, so nothing can be submitted while one is still there.

## Scope of this version

Frequentist analysis only. The Bayesian interim analysis is deliberately excluded for
now; there is a placeholder section and a decision to make (own section vs. companion
paper).

## The one substantive change to the existing sample size work

The derivation in `4_Projekte/Statistics_Resolve_Swiss/1_Sample_size_considerations.R`
feeds `sd = 0.93` into `ssc_meancomp()`. That 0.93 is the *precision-weighted spread of
the treatment effect estimate* pooled from Wälti and RESOLVE AUS — that is, uncertainty
about the true effect. The argument the function expects is the **between-participant SD
of the outcome**, which for the RMDQ at 18 weeks is about 5.2 points (RESOLVE AUS,
Table 2). Mixing the two makes the resulting *n* = 84/arm hard to defend in review: a
reviewer who recomputes it with σ = 5.2 gets a number roughly 30× larger. The same
script also tests a superiority margin (H₀: Δ ≤ 2) rather than H₀: Δ = 0, which is a
defensible but unusual choice that would need to be argued explicitly.

The manuscript therefore uses a conventional derivation:

- σ = 5.2 (pooled RMDQ SD at 18 weeks, RESOLVE AUS)
- target difference 2 RMDQ points, two-sided α = 0.05, 80 % power
- ANCOVA adjustment for baseline, assumed baseline–follow-up correlation 0.6
- design effect for clustering with unequal practice sizes (Eldridge 2006), cv = 0.65
  — Eldridge reports cv ≈ 0.65 for trials randomising general practices and says
  unequal size can be ignored only below cv = 0.23; physiotherapy practices are the
  closest analogue we have

→ 69/arm before clustering, **107/arm at ICC = 0.03**, i.e. 119 recruited per arm with
10 % attrition. The committed 100 analysed per arm covers an ICC up to about 0.03,
which is the honest way to state it.

Two approximations are named in the paper rather than hidden: the ANCOVA gain is applied
as the individual-level factor (1−r²) and the clustering inflation separately, where
Teerenstra et al. (2012) give a formula that handles both together (ours is mildly
conservative); and the ICC is assumed, not known, so everything is tabulated across a
range.

## What the power tables say about 5 vs 9 practices

Two findings worth taking to the project meeting:

1. **Rebalancing 5/9 to 7/7 buys almost nothing** — about 3 percentage points of power at
   every ICC considered, because the total analysed sample is what it is. Copas & Hooper
   (2021) show equal cluster allocation is optimal exactly when the ICC and total
   variance match across arms, so 7/7 is still the right default; the efficiency argument
   for changing it is just weak.
2. **The number of practices is the problem.** At 14 participants per practice, going
   from 5+5 to 7+7 practices lifts power from 62 % to 80 % (ICC 0.01), and 9+9 reaches
   90 %. van Breukelen & Candel (2018) recommend at least 10 clusters per arm and, as a
   rule of thumb, adding two to three clusters per arm to any normal-approximation
   sample size. This agrees with the July 2026 interim note in the project folder.

## Open questions for the trial team

1. Co-author list, affiliations, CRediT contributions.
2. Trial registration number and registry; ethics committee and approval number.
3. Full follow-up schedule — is 18 weeks the only follow-up, or are there 26/52-week
   time points as in Australia?
4. Complete list of secondary outcomes with instruments and time points.
5. Participant and **practice-level** eligibility criteria (a cluster trial needs both).
6. How were the 14 practices actually allocated? Simple, stratified, or
   covariate-constrained? This determines both the model adjustment and which
   allocations the randomisation test may enumerate.
7. Were practices recruited in waves, and is any wave confounded with arm?
8. Who is blinded, and will the statistician be blinded to arm labels until the primary
   analysis is locked?
9. How is contamination in control practices measured?
10. Adherence threshold for the "sufficient dose" definition.
11. Where will the analysis code live (repository URL for the data availability
    statement)?
12. Whether the Bayesian interim analysis goes into this paper or a companion one.
