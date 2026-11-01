# Research pipeline

One `targets` pipeline, organized by the four parts in `../docs/findings.md`.
Results are displayed in `reports/results.html`.

```r
testthat::test_file("tests/test_pipeline.R")   # unit tests on synthetic data
targets::tar_make()                            # runs whatever is out of date
targets::tar_outdated()                        # what would run, without running it
```

The first run downloads StatsBomb open data twice over: lineups and shots
(`build_shots.py`), then every match's full event file for the entry counts
(`build_events.py`). Both are cached under `data/statsbomb_raw/`; later runs
need no network. Copy an existing `data/statsbomb_raw/` folder in to skip the
first download.

| Section of `_targets.R` | What it computes |
|---|---|
| Data | Matches, open-play shots, team-match table, segments, identification diagnostics |
| A | Baseline M0 / M1 / M3 on shot counts; M4 (positions), MRoles (role interactions), MGraph (teammate graph) on the same folds |
| B | The baseline on goals, shots, box entries and final-third entries |
| C, D | Slots for interactions and the identifiability simulation |

## Files

```
_targets.R            the pipeline
build/build_shots.py  StatsBomb lineups, shots, stints, segments (cached)
build/build_events.py entries into the final third and the box, per segment
R/
  load.R, aggregate.R matches, shots, team-match table, team contrasts
  stints.R            stints and segments; the segment table
  identification.R    minutes, teammate overlap, precision, player design
  player_models.R     model data, ridge fits, cross-validation, reliability
  structure.R         M4, MRoles, MGraph (penalized IRLS)
  outcomes.R          Part B: outcomes per segment, CV by outcome, ladder figure
  helpers.R, pipeline.R
reports/results.Rmd   the one results report
tests/                unit tests
```

## Checks after the first run

- Part A baseline: players over teams 135.9 (SE 34.6), reliability 0.082 /
  0.115. These must match the earlier pipeline exactly.
- Part B shots row: identical to the Part A baseline (same data and folds).
- M4: 13.9 (SE 21.0) as before. MRoles and MGraph are rebuilt implementations,
  so their numbers can differ slightly from the first versions (-2.0 and 5.1).
