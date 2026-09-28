# Deliberation and Learning

Robert C. Luskin, Gaurav Sood, and James S. Fishkin

How much do participants in Deliberative Polls learn, does participation cause learning, and who gains most on the knowledge items?

[Manuscript](ms/main.pdf) · [Manuscript source](ms/main.Rmd) · [Results](tabs/) · [Figures](figs/)

The main analysis follows 8,800 participants in 592 discussion groups across 28 polls. It reports absolute gains, gains relative to initial knowledge, and estimates adjusted for guessing using the item model in [guess](https://github.com/soodoku/guess). Figure 1 also compares attendee and control gains in the two main-sample polls with both interviews in both arms. Table 1 adds education, age, and gender covariates across all 28 polls. The fourth column adds attitude extremity and group disagreement; the table notes report the standard-deviation alternative. The appendix checks groupmates’ knowledge of missed questions in the same 28 polls, and reports poll-specific estimates, precision-weighted averages, and later control interviews.

All confidence intervals use a hierarchical bootstrap: resample polls within mode, then discussion groups, keeping each participant's item responses and interview waves together. Within-poll comparisons hold the poll fixed and resample separately by study arm; independent controls are individual sampling units. Pooled learning estimates weight polls equally. Control comparisons can reflect selection into attendance, and groupmate coefficients are descriptive.

The [dp-data repository](https://github.com/soodoku/dp-data) supplies typed Parquet tables of polls, items, participants, responses, and scores. This repository joins and filters those tables; the poll and item appendices are generated from them. [Build provenance](tabs/provenance.json) records the data revision, input manifests, model package version, and bootstrap replicate count. Self-reported briefing reading is available for 12 polls and enters the third column of Table 1.

## Reproduce

Install R 4.6, Pandoc, and XeLaTeX. Clone `dp-data` beside this repository, then run:

```sh
git clone https://github.com/soodoku/dp-data.git ../dp-data
make restore
make check
```

`DP_DATA_ROOT` can point to another checkout. `make check` rebuilds the tables, figures, and PDF, then runs linting and numerical tests. The build checks inputs against the manifests in `dp-data`; it does not download data or keep a second source inventory here. `make ci-docker` runs the same checks in a standard R image. The analysis uses 999 bootstrap replicates; `DP_BOOT_WORKERS` controls the number of parallel workers (default: 2).

## Repository layout

- `R/` contains functions for reading data, computing measures, fitting models, and formatting results.
- `scripts/` contains the two entry points: `run_all.R` writes `tabs/`, and `figures.R` writes `figs/`.
- `ms/` contains the manuscript source and PDF.
- `tabs/` and `figs/` contain generated results.
- `tests/` checks source contracts and key numerical results.
