# Deliberation and Learning

Robert C. Luskin, Gaurav Sood, and James S. Fishkin

How much do participants in Deliberative Polls learn, does participation cause learning, and who gains most on the knowledge items?

[Manuscript](ms/main.pdf) · [Manuscript source](ms/main.Rmd) · [Results](tabs/) · [Figures](figs/)

The analysis includes polls with respondent-level knowledge-item answers before and after deliberation. It reports poll gains, attendee-control comparisons, and models of T2 knowledge conditional on T1 and group composition. The main pre/post models use scores rebuilt from item answers in 21 polls. Three polls have control groups. Those comparisons can reflect selection into attendance.

The [dp-data repository](https://github.com/soodoku/dp-data) holds the data and their provenance, poll metadata, source checksums, and generated-file manifests. This repository reads its typed Parquet poll, item, participant, item-response, and score tables. The [poll appendix data](tabs/polls.csv) gives each included poll one row. The item appendix is generated from the [canonical knowledge-item catalog](https://github.com/soodoku/dp-data/blob/main/metadata/items.csv). The regression table also examines groupmates' T1 answers to the questions each respondent got wrong at T1. The latent learning estimates use the source-survey batteries rebuilt in dp-data; [its comparison audit](https://github.com/soodoku/dp-data/blob/main/audit/knowledge_parity.csv) records differences from the Cor–Sood deposit.

The America in One Room, climate, and antimicrobial-resistance studies provide respondent item answers for both attendees and controls.

The upstream respondent export recovers briefing-material reading reports for nine polls. The regression table uses the source-linked reports from all nine.

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
