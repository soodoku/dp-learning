# Deliberation and Learning

Robert C. Luskin, Gaurav Sood, and James S. Fishkin

How much do participants in Deliberative Polls learn, does participation cause learning, and who gains most on the knowledge items?

[Manuscript](ms/main.pdf) · [Manuscript source](ms/main.Rmd) · [Results](tabs/) · [Figures](figs/)

The analysis includes 31 polls with respondent-level knowledge-item answers before and after deliberation. The main attendee analysis uses 28 polls with verified small-group identifiers: 8,800 participants in 592 groups. The appendix reports observed and guessing-adjusted gains across all 31. Four polls have attendee-control comparisons. Three have common T1/T2 interviews; Northern Ireland has a T3-only control comparison. The control comparisons can reflect selection into attendance. Baseline knowledge is not fully balanced across the expanded set of small groups, so groupmate coefficients are descriptive.

The [dp-data repository](https://github.com/soodoku/dp-data) holds the data and their provenance, poll metadata, group identifiers, source checksums, and generated-file manifests. This repository joins its typed Parquet poll, item, participant, item-response, and score tables into one attendee panel. The [poll appendix data](tabs/polls.csv) gives each included poll one row. The item appendix is generated from the [canonical knowledge-item catalog](https://github.com/soodoku/dp-data/blob/main/metadata/items.csv). Supplementary historical-poll models examine briefing reading and groupmates' T1 answers to the questions each respondent got wrong at T1. The latent learning estimates use the source-survey batteries rebuilt in dp-data; [its comparison audit](https://github.com/soodoku/dp-data/blob/main/audit/knowledge_parity.csv) records differences from the Cor–Sood deposit.

The America in One Room, climate, antimicrobial-resistance, and Northern Ireland studies provide respondent item answers for both attendees and controls.

The upstream respondent export recovers briefing-material reading reports for nine polls. Their model appears in the appendix.

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
