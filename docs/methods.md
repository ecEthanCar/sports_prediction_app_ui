# Methods

Data and the shot baseline are in `research/`; sections marked background describe archived work (git tag `archive-2026-10`).

## Data

StatsBomb open data, 2015/16, competitions 2 (Premier League), 11 (La Liga), 12 (Serie A) and 7 (Ligue 1), season 27. `research/build/build_shots.py` downloads events and lineups once (cached in `data/statsbomb_raw/`) and writes matches, shots, stints, segments and players to `data/raw/`. Each shot row records the 22 players on the pitch, reconstructed from Starting XI events, substitutions and red cards in event order.

- **Shots:** open play only (penalties, direct free kicks and direct corners dropped).
- **Stints:** same 22 players; end at substitutions, red cards and half ends. Temporary absences ignored; zero-length stints dropped.
- **Segments:** stints split at goals (a goal's shot stays in the segment before it) and at 15-minute clock edges within each period (stoppage time in the last bin). One row per segment and attacking side: shots, minutes, score difference, time bin, player counts, attacking and defending players.

## Player models

Outcome y = open-play shots in a segment row; offset log(minutes); log link.

- **M0:** league, home, game state (score difference capped at +/-2, crossed with the six 15-minute bins), player counts (11v11, 11v10, 10v11, other).
- **M1:** M0 + team attack and defense effects, sum to zero within each league.
- **M3:** M1 + an offensive and a defensive effect per player with 900+ minutes; players below 900 minutes share a pooled per-league column; goalkeepers get defensive effects only. Player effects are ridge-penalized; everything else is unpenalized.

**Ridge as a prior.** glmnet with `standardize = FALSE` and lambda scaled so the penalty equals a N(0, tau\^2) prior on each player effect (log scale), accounting for glmnet's rescaling of `penalty.factor`. A unit test checks the fit against the posterior mode computed directly.

**Cross-validation.** 10 folds of whole matches (seed 2026); tau chosen over a 20-point log grid from 0.01 to 0.5 by summed held-out Poisson deviance. Comparisons between models use the same folds; SE = sqrt(k) x SD of the fold differences.

**Reliability.** M3 refit on alternating matches (two halves of the season); player effects correlated across halves for players with 300+ minutes in each half. Full-season reliability by Spearman-Brown, 2r / (1 + r).

**Likelihood.** Poisson. The Pearson dispersion on segments (1.47) is an artifact of splitting at goals; the Poisson likelihood summed over a partition whose boundaries depend only on the past equals the point-process likelihood.

## Structured priors

- **Positional prior (M4):** counts of attacking and defending CB, FB and FW on the pitch (midfield the reference, goalkeepers excluded), unpenalized, so each player's effect is a deviation from his position's mean.
- **Teammate graph prior:** penalty on offensive effects beta'(I + kappa L)beta, L the normalized Laplacian of the teammate shared-minutes matrix, scaled so a column with penalty I has a N(0, tau\^2) prior. Fit by penalized IRLS with a sparse Cholesky solve and warm starts along the tau grid; kappa = 0 reproduces M3.
- **Role-level interactions (MRoles):** M4 plus the 15 pairwise products of the six position counts (within the attacking side, within the defending side, and attacking x defending), unpenalized. Each product asks whether one role's effect depends on how many of another role are on the pitch.

## Frequency ladder (Part B)

`research/build/build_events.py` downloads each match's full event file once and counts, per segment and team, completed open-play passes and carries that enter the final third (start x \< 80, end x \>= 80) and the penalty box (end x \>= 102 and 18 \<= y \<= 62, start outside), excluding corners, free kicks, kick-offs and goal kicks. The baseline (M0, M1, M3) is refit on goals, shots, box entries and final-third entries with the same rows, offset, folds and grid; Poisson likelihood throughout. Outcomes are compared by held-out gain in SE units and split-half reliability, against events per team per 90.

## Outcome ladder toward quality (background)

Same rows, offset, folds and grid; only y changes: goals, shots, location xG, context xG, StatsBomb xG (both of our xG outcomes are held-out predictions). Poisson for counts, quasi-Poisson for xG totals. The quality channel is a separate model: context xG on rows with at least one shot, offset log(shots). Prior SDs on the true scale = tau x sqrt(dispersion), with dispersion E(q\^2)/E(q) for totals and Var(q)/E(q) for quality.

## Shot-quality models (background)

- **Location kernel xG:** Nadaraya-Watson on (lateral offset, distance from the goal line), Gaussian kernel with one bandwidth, computed on a half-yard grid (goals and shots binned and smoothed separately, then divided). Bandwidth by held-out log loss over 10 match folds.
- **Freeze-frame features:** goalkeeper distance from the shooter; goalkeeper angle off the shooter-to-goal line; open goal share (part of the 8-yard mouth not covered by opponents between shooter and goal line, each a yard wide); nearest outfield defender (capped at 30 yards); header flag.
- **Context xG:** logistic regression on distance, distance\^2, angle and the freeze-frame features, fit on training folds.
