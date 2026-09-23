# Raw inputs

| File | Source | Licence |
|---|---|---|
| `polardata.tab`, `poll_indices.tab` | Luskin, Sood, Fishkin and Hahn, *Deliberative Distortions* replication data, [doi:10.7910/DVN/D7G1LO](https://doi.org/10.7910/DVN/D7G1LO) | CC0 1.0 |
| `cor-sood-replication.zip` | Cor and Sood, *Guessing and Forgetting* replication data, [doi:10.7910/DVN/HZHVCU](https://doi.org/10.7910/DVN/HZHVCU) | CC0 1.0 |
| `greece.csv` | The authors' 2014 analysis file (`agg_data.Rdata`, pollid 2000), first released here | CC0 1.0 |

`greece.csv` holds the Marousi, Greece Deliberative Poll (2006), in which PASOK chose its mayoral candidate ([CDD](https://cdd.stanford.edu/2006/deliberative-polling-in-the-municipality-of-marousi-greece/)). It has the same derived columns as `polardata.tab`: scores, group identifiers and demographics, but no item-level responses. Recodes are in `evidence/recode_ledger.csv`.

Hashes are checked by `verify_sources()` in `R/sources.R`.
