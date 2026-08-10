# RESOLVE Swiss — statistical analysis plan (manuscript)

Target journal: **Brazilian Journal of Physical Therapy**, article type *Clinical Trial
Protocol* — the same slot in the same journal where the Australian RESOLVE SAP appeared
(Bagg et al., Braz J Phys Ther 2021;25(1):103–111, doi 10.1016/j.bjpt.2020.06.002).

## Files

| File | What it is |
|---|---|
| `paper.tex` | the manuscript (LaTeX master) |
| `paper.pdf` | current build, 26 pages |
| `references.bib` | 46 entries, every DOI verified against Crossref on 2026-08-03 |
| `stuff/` | study protocol v2.0 — **gitignored**, see below |
| `R/sample_size.R` | produces every number in the *Sample size* section; base R only |
| `paper_for_annotation.docx` | Word version for co-author comments, built by `./build_docx.sh` |

Build with `pdflatex → bibtex → pdflatex → pdflatex` (plain `latexmk` trips over a stale
`.aux` if you delete build artefacts by hand — delete them all or none). After every PDF
build, run `./build_docx.sh` to regenerate `paper_for_annotation.docx` (needs pandoc and
poppler). The script renders the TikZ flow diagram from the PDF and splices it in as an
image, since pandoc cannot convert TikZ.

Open items that need input from the trial team are marked `\open{...}` in the source and
render in **red** in the PDF, so nothing can be submitted while one is still there.

## Scope of this version

Primary analysis is the **constrained longitudinal mixed model** (baseline in the
outcome vector, time and time-by-treatment fixed effects, no treatment main effect,
random intercepts for practice and participant). This matches the study protocol's
LMM structure; the constraint (equal baseline means, true by randomisation) recovers
the precision of baseline adjustment — demonstrated empirically in
`R/equivalence_constrained_vs_ancova.R`. A **supplementary Bayesian analysis** is
prespecified, with the informative prior built from the individual participant data
of the Australian trial; the Bayesian interim analysis follows protocol V3 and is
summarised in its own section.

**Data policy:** the Australian IPD live outside this repo
(`4_Projekte/Statistics_Resolve_Swiss/DATA_Resolve_AUS/`) and are only ever *read*
from there. `.gitignore` blocks `DATA*/`, `*.xlsx`, `*.csv`, `*.rds` as a guard —
trial data must never be committed or pushed.

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

## What the power tables say about 4 vs 9 practices

Two findings worth taking to the project meeting:

1. **Rebalancing 4/9 to 6/7 recovers about 6 percentage points** at every ICC
   considered, but no rebalancing restores the planned 80 % because the total analysed
   sample is what it is. Copas & Hooper (2021) show equal cluster allocation is optimal
   exactly when the ICC and total variance match across arms, so a balanced split remains
   the right default.
2. **The number of practices is the problem.** At 14 participants per practice, going
   from 5+5 to 7+7 practices lifts power from 62 % to 80 % (ICC 0.01), and 9+9 reaches
   90 %. van Breukelen & Candel (2018) recommend at least 10 clusters per arm and, as a
   rule of thumb, adding two to three clusters per arm to any normal-approximation
   sample size. This agrees with the July 2026 interim note in the project folder and
   with the recruitment-repair scenarios discussed in August 2026 (five new practices
   randomised as a batch bring power back above 80 % for ICC up to about 0.03).

## The protocol conflict that needs a decision

Protocol v2.0 states the primary hypothesis as **superiority by a margin**: H₀ is that
the intervention does *not* exceed usual physiotherapy by more than 2 RMDQ points,
tested **one-sided** at α = 0.05, with 110 per arm giving 80 % power.

Under that framing what drives power is how far the true effect exceeds 2 points, not
the effect itself. The 110/arm figure was obtained with an assumed true effect of ≈ 2.4
and an SD of ≈ 0.9 — and that 0.9 is the spread of the pooled effect estimates from
Wälti and RESOLVE AUS, i.e. uncertainty about the effect, not the between-participant SD
of the RMDQ (≈ 5.2). Recomputed with 5.2, a margin-based design powered for a 0.4-point
excess over a 2-point null needs **several thousand participants per arm**. The trial as
resourced cannot deliver it.

Read as a conventional superiority trial targeting a 2-point difference, 110/arm is
close to right. The manuscript therefore does that and says so explicitly in a section
"Hypothesis framing: a clarification of the protocol". **This needs the sponsor's
agreement**, and if the registered protocol wording is to change, a protocol amendment.
The alternative is to keep the margin formulation and state openly that the trial is
underpowered for it. That is a decision for Thomas Benz as sponsor-investigator, not one
the statistician should make alone.

Related and smaller: the protocol specifies a **one-sided** test for the primary outcome.
The manuscript prespecifies two-sided throughout (more conservative, and what reviewers
expect); also flagged for confirmation.

## Two further gaps in the protocol's statistical section

1. The protocol's model — arm, time, arm × time, time as ordered categorical — **does not
   mention a random effect for the practice**. Without it, patients in the same practice
   are treated as independent and the standard error is too small. Every model in the SAP
   carries a practice random intercept.
2. The protocol does not specify any **small-sample correction**, which with 14 clusters
   is not optional.

## Answered by protocol v2.0

Follow-up: physical outcomes at baseline and 18 weeks; all self-reported questionnaires,
including the RMDQ, also at **26 and 52 weeks**. Eligibility at both levels, the full
secondary outcome list, the intervention (12 sessions over 16 weeks), the randomisation
procedure (1:1, central coordinator at ZHAW, concealed, **no stratification mentioned**),
blinding (participants no, physical assessors yes via video), adherence (≥ 75 % for the
IV analysis), contamination control (training and materials withheld from control
therapists), and data capture (REDCap) are now all in the manuscript.

## Open questions for the trial team

1. Co-author list, affiliations, CRediT contributions, funder and grant number.
2. Registration numbers (ClinicalTrials.gov and SNCTP) once issued; ethics committee,
   approval number and date.
3. **Confirm the current split is 5 intervention vs 9 control** — the protocol plans 20
   practices at 1:1, so this is a reportable deviation either way.
4. **Confirm allocation was simple**, i.e. not stratified or covariate-constrained. If it
   was constrained, those variables must enter the model and the randomisation test may
   only enumerate the allocations satisfying the constraints.
5. Were practices recruited in waves, and is any wave confounded with arm?
6. Will the statistician be blinded to arm labels until the primary analysis is locked?
7. Data-cleaning and query workflow; who locks the database.
8. Is the health-economic analysis (QALYs, ICERs) out of scope for this paper?
9. Repository URL for the data availability statement.
10. Whether the Bayesian interim analysis goes into this paper or a companion one.

## A note on the protocol document

`stuff/250912_RESOLVE Swiss_study-protocol-V2_clean version.docx` carries a
confidentiality statement restricting transmission to the ethics committees and
regulatory authorities without the sponsor's written authorisation. It is therefore in
`.gitignore` and has **not** been pushed to GitHub, private repo or not. If the team
wants it version-controlled alongside the manuscript, that needs the sponsor's sign-off
first.
