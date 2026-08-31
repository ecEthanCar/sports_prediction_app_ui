# Double Poisson Modeler

An interactive Shiny app for teaching the double Poisson model for
football match outcomes. Built as the soccer module for an eCornell
sports analytics course.

Pick a league and season, and the app fits a Poisson GLM giving every
team an attack and a defense rating, then uses those ratings to predict
any fixture. Five tabs: a match outcome predictor with scoreline probabilities, 
a ratings table with standard errors, a quadrant map of the league's ratings,
and a worked explanation of the math driven by the numbers currently on screen.

Predictions are compared against what actually happened. The scoreline
the two teams really produced that season is outlined on the heatmap,
alongside the probability the model had assigned to it.

## Source

The model and the `contr.sum` workflow follow:

> Egidi, L., Karlis, D., and Ntzoufras, I. (2025). *Predictive Modelling
> for Football Analytics.* Chapman & Hall/CRC Data Science Series. ISBN
> 9781032030630.

Match data comes from the **footBayes** package, which the book was
written to accompany. This app implements only the frequentist double
Poisson model from the book's early chapters; footBayes itself extends
to bivariate Poisson, Bayesian, and dynamic formulations.

Two foundational papers behind the approach:

-   Maher, M. J. (1982). Modelling association football scores.
    *Statistica Neerlandica*, 36(3), 109-118.
-   Dixon, M. J. and Coles, S. G. (1997). Modelling association football
    scores and inefficiencies in the football betting market. *Journal
    of the Royal Statistical Society: Series C*, 46(2), 265-280.

## The model

Each team gets an attack rating and a defense rating. Goals are Poisson
with

```         
log(lambda_home) = mu + gamma + att[i] - def[j]
log(lambda_away) = mu         + att[j] - def[i]
```

subject to `sum(att) = 0` and `sum(def) = 0`. The sum-to-zero
constraint, applied through `contr.sum`, resolves the identifiability
problem: without it you could add a constant to every attack rating,
subtract it from every defense rating, and get identical predictions. It
also makes 0 mean "league average" so ratings are directly comparable.

Fitted separately per season. No time weighting within a season, and no
carryover between them.

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

Nothing in `data.R`, `model.R`, `plots.R`, `labels.R`, or `docs.R`
references `input`, `output`, or a reactive. That is deliberate:
`lesson_script.R` calls the same functions the app does, so the console
walkthrough and the app can never drift apart.

## Dependencies

``` r
install.packages(c(
  "shiny", "bslib",                        # app framework and theming
  "dplyr", "tidyr", "tibble", "stringr",   # data manipulation
  "ggplot2", "ggrepel", "scales"           # plotting
))

install.packages("footBayes")              # match data (optional, see below)
```

`stats` and `utils` are base R. Fitting uses `stats::glm`, so no
modelling package is required.

Three things load from the network at runtime:

-   **footBayes** supplies the league datasets. Without it the app falls
    back to a simulated 20-team league so it still runs end to end, but
    those ratings are fitted to fake data and should not be used in
    course materials.
-   **Google Fonts** (IBM Plex Sans, Barlow Condensed, IBM Plex Mono).
    Offline, the app falls back to system faces and still looks
    intentional.
-   **MathJax** typesets the equations on the explanation tab. Offline,
    formulas render as raw TeX; the ladders and prose still carry the
    explanation.

None of the three is required for the model itself.

## Running it

Put every file in one directory. From its parent:

``` r
shiny::runApp("app")
```

Or open `ui.R` or `server.R` in RStudio and use the Run App button.

For the console version, set the working directory to the app folder
first, since it sources the support files by relative path:

``` r
setwd("app")
source("lesson_script.R")
```

## Adding a league

One row in `league_registry()` in `data.R`:

``` r
"Spanish La Liga", "footBayes", "spain", "soccer", NA_real_
```

Rows whose datasets are not installed are dropped silently at startup,
so a registry entry for something unavailable is harmless. Add a
name-shortening pattern to `TEAM_LABEL_RULES` in `labels.R` at the same
time, or long club names will crowd the quadrant map.

## Current limits

-   **Ratings are in-sample.** The model is fitted on a full season and
    then used to "predict" matches from that same season. Fine for
    teaching what the parameters mean, but not a measure of forecasting
    accuracy. Walk-forward validation, refitting after each matchweek,
    is the honest version and is not implemented.
-   **Scores are assumed independent.** Multiplying the two Poisson
    margins understates draws and 0-0s. Dixon-Coles corrects this and is
    the natural next extension.
-   **Partial seasons fail loudly.** If a team never appears in one of
    the home or away roles, the fit is rank deficient and
    `extract_ratings()` stops with an explanatory message rather than
    returning wrong numbers.
-   **The scoreline grid is truncated** at 8 goals per side and
    renormalized, so outcome probabilities sum to exactly 1.
