# Deliberation and Learning

Robert C. Luskin, Gaurav Sood, and James S. Fishkin

How much do participants in Deliberative Polls learn, does participation cause learning, and who gains most on the knowledge items?

[Manuscript](ms/main.pdf) · [Manuscript source](ms/main.Rmd) · [Results](tabs/) · [Figures](figs/)

The main analysis compares pre-arrival and immediate-exit knowledge among attendees. It reports absolute gains, gains relative to initial knowledge, and estimates adjusted for guessing using the item model in [guess](https://github.com/soodoku/guess). Figure 1 also compares attendee and control gains in the two studies with both interviews in both arms. The regression table adds demographic covariates, then separately adds attitudes and briefing reading. The appendix examines groupmates' knowledge of missed questions, poll-specific estimates, precision-weighted averages, and later control interviews. Current sample sizes and estimates come from the [generated results](tabs/) and appear in the manuscript.

All confidence intervals use a hierarchical bootstrap: resample polls within mode, then discussion groups, keeping each participant's item responses and interview waves together. Within-poll comparisons hold the poll fixed and resample separately by study arm; independent controls are individual sampling units. Pooled learning estimates weight polls equally. Control comparisons can reflect selection into attendance, and groupmate coefficients are descriptive.

The [dp-data repository](https://github.com/soodoku/dp-data) supplies typed Parquet tables of polls, items, participants, responses, and scores. This repository joins and filters those tables; the poll and item appendices are generated from them. [Build provenance](tabs/provenance.json) records the data revision, input manifests, model package version, and bootstrap replicate count. Self-reported briefing reading enters the fourth column of the regression table; attitudes enter the third.

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

The phase tables in `tabs/phase_contrasts.csv` distinguish exit minus arrival,
arrival minus pre-arrival, and exit minus pre-arrival. Each comparison reports
its available observed-wave sample and a common three-wave sample where one
exists. All-three contrasts share bootstrap draws; their raw changes add up.
`tabs/phase_coverage.csv` records missing phases and unknown questionnaire
presence, attendance and groups. An interim questionnaire is not treated as
arrival, and a missing arrival wave never falls back to pre-arrival.

`tabs/pre_arrival_selection.csv` reports descriptive pre-arrival knowledge by
recorded attendance and study arm, separate attendee comparisons with recruitment
nonattenders, invited nonattenders and uninvited controls, and exit attrition among known attendees.
Unknown attendance stays unknown. These comparisons describe selection; they
do not remove unobserved selection or establish causal deliberation effects.
The input phases, questionnaire states and source population come from dp-data;
this reader performs no respondent recoding. Separate deposits remain separate
batteries rather than being pooled as independent studies.

Scores and changes are proportions correct; multiply a change by 100 for
percentage points. `tabs/phase_provenance.json` records the upstream revision,
source checksums and bootstrap count for these tables. They supplement the
main pre-arrival-to-exit analysis. Original questionnaire numbers are mapped
explicitly to event stages upstream. Missing phases mean unavailable comparisons in the verified
export, not proof that a poll never collected that questionnaire. Matching
batteries and denominators are required; the California eight-item arrival/exit
battery is not mixed with its five-item telephone/exit battery. Marousi paired
changes preserve the authored source merge, whose sixteen conflicting exit IDs
remain an upstream linkage limitation.

The phase estimates are unweighted sample changes, without guessing adjustment.
Seven Cor-Sood cohorts now have source-verified attendance in the phase
participant export. Other cohorts retain unresolved attendance and contribute
coverage; historical counterparts supply comparisons where available.
A hardcoded participant arm or an online post survey alone is insufficient
evidence of attendance. Comparable three-wave scores now support California,
Europolis, Marousi, Michigan and Tomorrow's Europe. Michigan uses its common
partial battery. The upstream wave catalog also records arrival materials for
Denmark and Vermont; those polls do not enter the three-wave decomposition.
Counts refer to underlying studies, so the two Primaries catalog IDs do not
count twice.

The manuscript presents the common three-wave decomposition and pre-arrival
selection gaps in the main text. Its phase appendix reports available paired
estimates and exit attrition. The main models use pre-arrival and exit in every poll. Arrival-to-exit estimates
remain separate from these combined preparation-and-event gains.
All manuscript numbers and phase exhibits read the generated replication tables.

Wave timing, original survey labels, questionnaire presence, attendance evidence,
session counts and study identities are read from typed `dp-data` Parquet tables.
This repository selects analysis samples and estimates learning; it does not
reconstruct those source facts. The approved 2004 Primaries analysis includes one
study with 239 observed paired attendees (238 with known groups), excluding the
overlapping historical subset and people with no recorded discussion attendance.

`tabs/retention.csv` compares baseline, exit and delayed follow-up among the same
attendees in NIC 1996 and the 2021 climate poll. It requires observed questionnaires
and a common battery at all three interviews, reports group-bootstrap intervals,
and does not impose the guessing model's no-forgetting assumption. NIC's main
analysis uses its documented exit interviews; the ten-month follow-up is retained
only for this separate comparison. All source items and interview timing remain
in dp-data.

The focused attrition comparison (`make attrition`) uses upstream typed phase and
participant tables for Marousi, NIC and the climate poll. `tabs/attrition_flow.csv`
and `tabs/attrition_patterns.csv` record observation counts and intermittent
missingness; `tabs/attrition_recruitment.csv` retains distinct attendance and
assignment categories. `tabs/attrition_means.csv` records model predictors,
response probabilities, weight diagnostics and baseline differences.
`tabs/attrition_contrasts.csv` compares complete-case, normalized IPW and augmented
IPW point estimates. Marousi targets baseline-observed attendees; follow-up
comparisons target baseline-and-exit-observed attendees. These adjustments assume
response is independent of missing knowledge conditional on recorded predictors.
They do not estimate sampling intervals, nonresponse sensitivity bounds or
invitation effects. The manuscript reports the estimates and their assumptions.

The climate poll retains its published cohort of 962 people who completed the
sessions and post-survey, labeled `completed` upstream. The other 7,018 invitees
are `invited_noncompleter`; their attendance is unknown in the analytical view.
Baseline comparisons therefore describe completers versus other invitees.
Neither this labeling nor the nullable attendance flag changes paired knowledge
scores, the 962-person treatment cohort, or the 671-person post-survey control
cohort. America in One Room 2019 retains its separate attendance definition.

`tabs/knowledge_gaps.csv` compares pre-arrival and exit gender and education gaps
on the demographic-model sample. Each comparison weights eligible polls equally;
`tabs/knowledge_gaps_by_poll.csv` preserves its poll-specific means and counts.
Intervals resample whole polls within mode and preserve both interviews. These
are descriptive changes in gaps, distinct from baseline-adjusted gain coefficients.
The relative-education comparison uses the upstream within-poll median split
in the 20 historical polls with that definition. Medians use each poll's reviewed
ordered education measure among unique historical participants; tied categories
remain together. Fixed qualification comparisons retain their separate coverage. The regression education
categories are unchanged.
