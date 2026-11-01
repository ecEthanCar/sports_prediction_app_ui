# Soccer analytics: course module and player-attribution research

Two pieces of work for Professor Martin Wells's eCornell sports analytics
course and a possible paper.

| Folder | What it is |
|---|---|
| `app/` | **Double Poisson Modeler**, a Shiny app teaching the double Poisson model for match outcomes (the course module) |
| `course_data/` | The EPL shot dataset built for the course, with its codebook entry |
| `research/` | Player-effects pipeline on StatsBomb open data: segments of play, plus-minus models, structured priors, frequency ladder |
| `docs/` | `findings.md` (every result with its numbers), `methods.md`, `references.md` |

Earlier work (the Understat analysis, the full StatsBomb pipeline with team
models, kernel xG, freeze-frame xG and the outcome ladder) is preserved in the
git tag `archive-2026-10`. Its results are recorded in `docs/findings.md`.

## The research question

Can individual and interaction effects be estimated from soccer lineup data,
and what does it take? Four parts:

- **A. Shot baseline (done):** players add predictive value beyond their teams
  (about 4 SEs of held-out gain), but individual effects barely replicate
  across halves of the season (reliability about 0.1), and no structured prior
  (positions, role interactions, a teammate graph) improves on plain ridge.
- **B. Frequency ladder (next):** the same models on more frequent outcomes
  (box and final-third entries), testing whether reliability rises as it does
  in hockey.
- **C. Interactions:** on the outcome where players are reliable, if B succeeds.
- **D. Identifiability simulation:** how much signal this lineup design can
  support.

Details and every number: `docs/findings.md`.

## Running the app

```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "tibble", "stringr",
                   "ggplot2", "ggrepel", "scales", "footBayes"))
shiny::runApp("app")
```

The model follows Egidi, Karlis and Ntzoufras (2025), *Predictive Modelling for
Football Analytics*: every team gets an attack and a defense rating with
sum-to-zero constraints, fit as a Poisson GLM per season. `app/lesson_script.R`
is the console version for course material. Known limits: ratings are
in-sample, and the two scores are treated as independent (Dixon-Coles is the
natural extension).

## Running the research pipeline

Requires R (`targets`, `tarchetypes`, `Matrix`, `MASS`, `glmnet`, `dplyr`,
`readr`, `tibble`, `ggplot2`, `testthat`; `crew` optional) and Python 3 with
`requests`.

```r
setwd("research")
testthat::test_file("tests/test_pipeline.R")
targets::tar_make()          # first run downloads StatsBomb data once
```

Results are displayed in `research/reports/results.html`, which the pipeline
re-knits whenever a result changes. The cache in `research/_targets/` is
binary; read any result with `targets::tar_read(name)`.
