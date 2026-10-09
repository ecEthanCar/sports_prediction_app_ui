# Double Poisson Modeler

An interactive Shiny app for teaching the double Poisson model for football match outcomes. Built as the soccer module for an eCornell sports analytics course.

Pick a league and season, and the app fits a Poisson GLM giving every team an attack and a defense rating, then uses those ratings to predict any fixture. Five tabs: a match outcome predictor with scoreline probabilities, a ratings table with standard errors, a quadrant map of the league's ratings, and a worked explanation of the math driven by the numbers currently on screen.

Predictions are compared against what actually happened. The scoreline the two teams really produced that season is outlined on the heatmap, alongside the probability the model had assigned to it.

The repository also holds a research pipeline in `stint_research/` that extends the module from goals to shots and lineups; see [StatsBomb research pipeline](#statsbomb-research-pipeline) below.

## Source

The model and the `contr.sum` workflow follow:

> Egidi, L., Karlis, D., and Ntzoufras, I. (2025). *Predictive Modelling for Football Analytics.* Chapman & Hall/CRC Data Science Series. ISBN 9781032030630.

Match data comes from the **footBayes** package, which the book was written to accompany. This app implements only the frequentist double Poisson model from the book's early chapters; footBayes itself extends to bivariate Poisson, Bayesian, and dynamic formulations.

Two foundational papers behind the approach:

- Maher, M. J. (1982). Modelling association football scores. *Statistica Neerlandica*, 36(3), 109-118.
- Dixon, M. J. and Coles, S. G. (1997). Modelling association football scores and inefficiencies in the football betting market. *Journal of the Royal Statistical Society: Series C*, 46(2), 265-280.

## The model

Each team gets an attack rating and a defense rating. Goals are Poisson with

```         
log(lambda_home) = mu + gamma + att[i] - def[j]
log(lambda_away) = mu         + att[j] - def[i]
```

subject to `sum(att) = 0` and `sum(def) = 0`. The sum-to-zero constraint, applied through `contr.sum`, resolves the identifiability problem: without it you could add a constant to every attack rating, subtract it from every defense rating, and get identical predictions. It also makes 0 mean "league average" so ratings are directly comparable.

Fitted separately per season. No time weighting within a season, and no carryover between them.

## Files

| File | Purpose |
|------------------------------------|------------------------------------|
| `global.R` | Packages, sources the support files, builds the league list |
| `ui.R` | Layout, sidebar controls, tab structure |
| `server.R` | Reactive layer; chains the pure functions below |
| `data.R` | League registry, dataset discovery, season filtering |
| `model.R` | The model: fitting, rating extraction, scorelines, head-to-head |
| `plots.R` | Quadrant map and scoreline heatmap builders |
| `labels.R` | Per-league team name shortening, season labels |
| `theme.R` | bslib theme and the app's CSS |
| `docs.R` | The "How the model works" panel |
| `lesson_script.R` | Console version, no Shiny; for use as course material |

Nothing in `data.R`, `model.R`, `plots.R`, `labels.R`, or `docs.R` references `input`, `output`, or a reactive. That is deliberate: `lesson_script.R` calls the same functions the app does, so the console walkthrough and the app can never drift apart.

## Dependencies

``` r
install.packages(c(
  "shiny", "bslib",                        # app framework and theming
  "dplyr", "tidyr", "tibble", "stringr",   # data manipulation
  "ggplot2", "ggrepel", "scales"           # plotting
))

install.packages("footBayes")              # match data (optional, see below)
```

`stats` and `utils` are base R. Fitting uses `stats::glm`, so no modelling package is required.

Three things load from the network at runtime:

- **footBayes** supplies the league datasets. Without it the app falls back to a simulated 20-team league so it still runs end to end, but those ratings are fitted to fake data and should not be used in course materials.
- **Google Fonts** (IBM Plex Sans, Barlow Condensed, IBM Plex Mono). Offline, the app falls back to system faces and still looks intentional.
- **MathJax** typesets the equations on the explanation tab. Offline, formulas render as raw TeX; the ladders and prose still carry the explanation.

None of the three is required for the model itself.

## Running it

Put every file in one directory. From its parent:

``` r
shiny::runApp("app")
```

Or open `ui.R` or `server.R` in RStudio and use the Run App button.

For the console version, set the working directory to the app folder first, since it sources the support files by relative path:

``` r
setwd("app")
source("lesson_script.R")
```

## Adding a league

One row in `league_registry()` in `data.R`:

``` r
"Spanish La Liga", "footBayes", "spain", "soccer", NA_real_
```

Rows whose datasets are not installed are dropped silently at startup, so a registry entry for something unavailable is harmless. Add a name-shortening pattern to `TEAM_LABEL_RULES` in `labels.R` at the same time, or long club names will crowd the quadrant map.

## Current limits

- **Ratings are in-sample.** The model is fitted on a full season and then used to "predict" matches from that same season. Fine for teaching what the parameters mean, but not a measure of forecasting accuracy. Walk-forward validation, refitting after each matchweek, is the honest version and is not implemented.
- **Scores are assumed independent.** Multiplying the two Poisson margins understates draws and 0-0s. Dixon-Coles corrects this and is the natural next extension.
- **Partial seasons fail loudly.** If a team never appears in one of the home or away roles, the fit is rank deficient and `extract_ratings()` stops with an explanatory message rather than returning wrong numbers.
- **The scoreline grid is truncated** at 8 goals per side and renormalized, so outcome probabilities sum to exactly 1.

## StatsBomb research pipeline

The `stint_research/` folder is the research follow-up to the course module: it models shots rather than goals and tests whether player and lineup effects can be estimated from soccer data. It uses StatsBomb open data for the four complete 2015/16 leagues (Premier League, La Liga, Serie A, Ligue 1): 1,517 matches and 35,732 open-play shots.

| Stage | What it does | Key results |
|-------------------|------------------------|------------------------------|
| 1\. Shot volume and quality | Splits team-match xG into shot volume (negative binomial) and quality per shot (three-component mixture) | Volume drives about 60% of team differences in xG. About 10% of shots are close-range big chances that convert 42%. StatsBomb xG underrates direct free kicks (113 goals vs 69.5 expected), so only open-play shots are used. |
| 2\. Stints and identification | Splits matches into stints (same 22 players on the pitch) and checks whether the data can separate players | 11,579 stints. Few players are never apart, but a typical player's effect is estimated less precisely (SE 19% of shots per 90) than the whole spread of team effects (SD 15%). |
| 3\. Player models | Ridge-penalized player effects on team shot volume, compared out of sample with a team-only model | Players add information beyond teams (gain of 3.5 SEs in held-out deviance), but individual effects barely replicate across halves of the season (r about 0.1). |
| 3b. Time controls and positions | Splits stints at goals and 15-minute marks; adds position groups | Preliminary, rerun pending: a positional prior adds nothing measurable, and about a third of the forward effect is substitution timing. |

Next: shot quality (xG, big chances) as the outcome, and a structured (Laplacian) penalty that uses who plays with whom, compared against ridge.

### Running it

The whole analysis is one `targets` pipeline. From the repository root:

``` r
setwd("stint_research")
testthat::test_file("tests/test_pipeline.R")   # unit tests, synthetic data
targets::tar_make()                            # runs everything out of date
```

Each step is cached and reruns only when its inputs or code change, so later runs are fast. The first run downloads about 1,500 matches from StatsBomb's open-data repository (cached in `data/statsbomb_raw/`); it needs Python 3 with `requests`, and the R packages listed in `stint_research/README.md`.

### Finding the results

- **Reports**: `tar_make()` renders HTML reports into `stint_research/reports/`: `1_team_models`, `2_stints`, `3_players`, `3b_positions`, and `figures`. They are generated, not committed.
- **Figures for slides**: PNGs in `stint_research/figures/`.
- **Any single result**: `targets::tar_read(cv3)` (any step name); `targets::tar_visnetwork()` shows every step and its dependencies.
- **Decisions and evidence**: `stint_research/README.md`.
