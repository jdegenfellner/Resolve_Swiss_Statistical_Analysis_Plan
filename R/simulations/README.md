# Claim-by-claim simulations

Every formula-based statement in the manuscript is backed here by a
Monte Carlo simulation. Run everything from the repository root with
`Rscript R/simulations/run_all.R` (about 10–20 minutes at the default
settings; `NSIM_FAST`, `NSIM_LM`, `NSIM_KR` scale the effort). Each
script prints the paper's claim next to the empirical value with its
Monte Carlo standard error and writes a figure to
`R/simulations/figures/`.

| Paper claim (section) | Value in paper | Script |
|---|---|---|
| Individually randomised trial needs 108/arm (Sample size) | power 0.80 | `01_individual_sample_size.R` |
| Baseline factor 1−r² reduces this to 69/arm (Sample size) | power 0.80 | `01_individual_sample_size.R` |
| Change scores less precise than ANCOVA at r = 0.6 (The model) | SE ratio 1.118 | `01_individual_sample_size.R` |
| Margin design needs well over a thousand per arm (Hypothesis framing) | ~0.80 at n = 1355 | `01_individual_sample_size.R` |
| Design effect eq. (1): 1.20 / 1.61 / 2.02 at ICC .01/.03/.05 (Sample size) | variance inflation | `02_design_effect.R` |
| Table 1 chain: requirement = 69 × DE → 83/111/139 | variance identity | `02_design_effect.R` |
| Table 2, row 4/9: power 0.73 / 0.61 / 0.51 at N = 200 | KR analysis | `03_power_current_design.R` |
| Table 2, balanced: 5/5 → 0.62, 7/7 → 0.80 (ICC .01), 10/10 → 0.85 (ICC .03) | KR analysis | `03_power_current_design.R` |
| Green passage: factorisation neutral at ρc = 0.6, within 20% for ρc 0.4–0.8 (Sample size) | requirement ratio | `04_teerenstra_factorisation.R` |
| Naive inference understates uncertainty at 13 practices (Small clusters) | 0.05 nominal | `05_small_sample_inference.R` |
| **Finding, not a claim:** with a time-varying practice effect (ρc < 1) the rigid primary model inflates the type I error and KR does not repair it; the practice-by-time random effect (sensitivity analysis 6) restores the level with df near 11 | see script output | `05_small_sample_inference.R` |
| ICC formula τ²/(τ²+ν²+σ²) is the right one for model (2) (Intra-cluster correlation) | unbiased | `06_icc_and_change_penalty.R` |
| Constrained model matches baseline-adjusted precision (The model) | SE ratio | `06_icc_and_change_penalty.R` and `R/equivalence_constrained_vs_ancova.R` |

Data-based statements (the Australian SDs, the correlation r ≈ 0.5, the
beta-binomial fit) are not simulations; they are reproduced from the
IPD by `R/aus_*.R`, which read from the institute share only.

`00_helpers.R` holds the shared generative model, the world of the
paper's model (2) extended by a cluster autocorrelation ρc: the
practice effect has a time-constant and a time-specific part, the ICC
and the individual baseline–follow-up correlation r stay fixed. ρc = 1
is the literal model-(2) world, in which the practice effect cancels
from the treatment contrast; ρc = 0.6 is the neutral world in which
the paper's factorised planning formula is exact. Power and type I
error are reported in both. Practice sizes have mean 200/13 and
coefficient of variation 0.65, as in the paper.
