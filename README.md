# RESOLVE Swiss — statistical analysis plan

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22655090.svg)](https://doi.org/10.5281/zenodo.22655090)

This repository holds the statistical analysis plan for the RESOLVE Swiss trial and the
R code that reproduces every number reported in it.

RESOLVE Swiss is a two-arm parallel-group cluster randomised controlled trial in Swiss
physiotherapy practices. It compares graded sensorimotor retraining combined with pain
science education against usual physiotherapy for people with chronic non-specific low
back pain. The practice is the unit of randomisation and the participant is the unit of
analysis. The primary outcome is back-specific disability, measured with the
Roland–Morris Disability Questionnaire at 18 weeks after the baseline measurement.

The plan was written before any outcome data were analysed. It follows the guideline of
Gamble et al. (2017) for the content of statistical analysis plans and the CONSORT
extension for cluster randomised trials.

Trial registration: German Clinical Trials Register [DRKS00039237](https://drks.de/search/en/trial/DRKS00039237),
registered 4 February 2026. Swiss BASEC number 2025-00784.

## Reproducing the numbers

`R/sample_size.R` is the single source of every sample size, design effect and power
value in the manuscript. It uses base R only.

```bash
Rscript R/sample_size.R
```

Each number is checked twice over. `R/simulations/09_verify_paper_numbers.R` puts the
closed-form value from `R/sample_size.R` next to an independent Monte Carlo simulation of
the underlying data generating process, and reports any disagreement:

```bash
Rscript R/simulations/09_verify_paper_numbers.R     # about two minutes
```

It covers all four cells of Table 1, all eighteen of Table 2, all thirty-three of Table 3,
the standard error behind the power figure, and the quantities of the Bayesian section.

The wider simulation suite checks the individual claims of the text rather than the
tables. Run it from the repository root:

```bash
Rscript R/simulations/run_all.R                     # about 10–20 minutes
```

`NSIM_FAST`, `NSIM_LM` and `NSIM_KR` scale the effort. `R/simulations/README.md` lists
which script backs which claim.

## Building the manuscript

```bash
pdflatex paper && bibtex paper && pdflatex paper && pdflatex paper
```

`latexmk` trips over a stale `.aux` if build artefacts are deleted by hand, so delete all
of them or none. `./build_docx.sh` produces a Word version through pandoc; it needs
pandoc and poppler, resolves the cross-references from `paper.aux` and renders the TikZ
flow diagram from the PDF.

## Layout

| Path | Contents |
|---|---|
| `paper.tex`, `paper.pdf` | the manuscript and its current build |
| `references.bib` | the bibliography |
| `R/sample_size.R` | every number in the *Sample size* section |
| `R/power_figure.R`, `R/sample_size_surface.R` | the power figures |
| `R/simulations/` | the verification suite, one script per group of claims |
| `R/power_dashboard/` | a Shiny app for exploring the design space |
| `R/aus_*.R` | the analyses of the Australian trial data behind the Bayesian priors |
| `figures/` | figures used by the manuscript |
| `suppl_material/` | the completed Gamble 2017 checklist |

## Data

The individual participant data of the Australian RESOLVE trial inform the priors of the
Bayesian analysis. Those data belong to the investigators of that trial, are held under a
data sharing agreement, and are **not** part of this repository. The scripts under
`R/aus_*.R` read them from a local path and will not run without them.

The participant-level data of RESOLVE Swiss will be published separately once the trial
reports, under their own DOI and with a codebook. Section *Data and code availability* of
the manuscript sets out which variables that file will carry.

## Licence

The manuscript, its figures and the supplementary material are under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). The code under `R/` and the
build scripts are under the MIT licence. See [`LICENSE`](LICENSE).

## Citation

See [`CITATION.cff`](CITATION.cff). The archived version of record carries a DOI on
Zenodo.
