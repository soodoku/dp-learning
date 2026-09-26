# Deliberation and Learning

Robert C. Luskin, Gaurav Sood, and James S. Fishkin

How much do participants in Deliberative Polls learn, does participation cause learning, and who gains most on the knowledge items?

[Manuscript](ms/main.pdf) · [Manuscript source](ms/main.Rmd) · [Results](tabs/) · [Figures](figs/)

The analysis combines participant-level pre/post scores from 22 earlier polls, respondent-linked item answers from 21 of those polls, a separate 23-poll item battery replication, and four studies with control groups. The manuscript reports poll gains, treatment-control comparisons, and models of T2 knowledge conditional on T1 and group composition.

The [dp-data repository](https://github.com/soodoku/dp-data) holds the data and their provenance, poll metadata, source checksums, and generated-file manifests. This repository reads those upstream records. The [poll appendix data](tabs/polls.csv) is generated from the upstream poll registry, observed participant data, and item-score tables. Table 2's missed-item peer measure uses groupmates' T1 answers to the items each participant missed. The upstream source reconstruction reproduces both historical score waves for all 21 polls; Marousi has scores but no recovered individual item answers. The Cor–Sood deposited item batteries are analyzed separately for the latent learning estimates.

The America in One Room, climate, and antimicrobial-resistance analysis files also contain respondent item answers. The Tanzania file used here supplies a standardized knowledge index.

## Reproduce

Install R 4.6, Pandoc, and XeLaTeX. Clone `dp-data` beside this repository, then run:

```sh
git clone https://github.com/soodoku/dp-data.git ../dp-data
make restore
make check
```

`DP_DATA_ROOT` can point to another checkout. `make check` rebuilds the tables, figures, and PDF, then runs linting and numerical tests. The build checks inputs against the manifests in `dp-data`; it does not download data or keep a second source inventory here. `make ci-docker` runs the same checks in a standard R image.

## Repository layout

- `R/` contains functions for reading data, computing measures, fitting models, and formatting results.
- `scripts/` contains the two entry points: `run_all.R` writes `tabs/`, and `figures.R` writes `figs/`.
- `ms/` contains the manuscript source and PDF.
- `tabs/` and `figs/` contain generated results.
- `tests/` checks source contracts and key numerical results.
