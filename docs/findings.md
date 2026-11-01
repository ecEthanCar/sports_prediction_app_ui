# Findings

Every result in the project, with its numbers, organized by the four parts of the research plan. Results whose code is no longer in `research/` point to the git tag `archive-2026-10` (`archive: ...`).

Data throughout: StatsBomb open data, 2015/16, Premier League, La Liga, Serie A and Ligue 1 (1,517 matches; Ligue 1 has 377 of 380), open-play shots only (35,732). Gains are held-out Poisson deviance summed over 10 folds of whole matches, with fold-to-fold standard errors. Reliability is the correlation of player effects fit separately on alternating matches, for players with 300+ minutes in each half.

**The question:** can individual and interaction effects be estimated from soccer lineup data, and what does it take?

| Part | Question | Status |
|------------------------|------------------------|------------------------|
| A. Shot baseline | How much do players add on shots, and does structure help? | Done: players add about 4 SEs beyond teams; reliability about 0.1; no structured prior improves on ridge |
| B. Frequency ladder | Does reliability rise with more frequent outcomes, as in hockey? | Done: offense rises from 0.08 (shots) to 0.15 (final-third entries, four times the events); defense does not rise; no outcome reaches 0.3 |
| C. Interactions | On the most reliable outcome, do combinations matter? | Done: no structured model or role interaction beats ridge on final-third entries |
| D. Identifiability simulation | What reliability can this lineup design support? | Done: planted effects of 3% per 90 reproduce the real reliabilities; usable ratings need about 150+ events per 90 or several seasons |

## Conclusions

1.  **Player effects on attacking output exist and are small:** about 3% of a team's rate per 90. Players add predictive value beyond their teams on every outcome tried (2.5 to 4.8 SEs of held-out gain), and the cross-validated prior SD (3.5% on shots, 2.8% on entries) matches the effect size that reproduces the real results in simulation.
2.  **The observed reliability is what effects that size give on soccer's lineup structure.** The simulation, with known effects planted on the real lineups, reproduces the real split-half reliabilities on shots (offense and defense) and on final-third entries (offense) within replicate noise.
3.  **Usable individual ratings need far more information than one season of shots provides.** In simulation, one season reaches split-half reliability of about 0.31 at 150 events per 90 and about 0.60 at 1,000 (the design ceiling for one season); four seasons of entry-level data reach about 0.43 to 0.47 (about 0.6 full-data), even without roster turnover.
4.  **Interactions are out of reach in one season.** Role interactions, a positional prior and a teammate graph add nothing on shots or on entries; second-order effects are smaller than the main effects, which are already hard to recover.
5.  **Structured priors cannot create signal that is not in the data.** Every structured model was within about 1.7 SEs of plain ridge.

Background results (team output, shot quality, the xG outcome ladder) are at the end.

## Data decisions

- **StatsBomb rather than Understat**, because it records every player on the pitch for every shot. On EPL 2015/16 the two agree on shot counts (team-match correlation 0.991) and broadly on xG (0.896).
- **Open-play shots only.** StatsBomb's 2015/16 xG underrates direct free kicks (113 goals vs 69.5 expected, ratio 1.63); open play is calibrated (goals / xG = 1.03). Dropping set pieces removes 4.7% of shots. archive: `R/checks.R` (`xg_calibration_by_type`), `reports/1_team_models.Rmd`.
- **Bundesliga 2015/16 (34 matches) and EPL 2003/04 (Arsenal only) excluded** as partial seasons.

## A. Shot baseline

### Identification

- **Stints:** 11,579 stretches with the same 22 players, ending at substitutions, red cards and half ends. **Segments:** 21,644, which also split at goals and 15-minute clock edges so the score and time bin are exact (43,288 rows, one per attacking side). Segment totals reproduce the team-match table exactly.
- **Imprecision, not inseparability.** Few teammate pairs are never apart, but a typical player's effect has an SE of about 19% of shots per 90 without shrinkage, against a spread of team effects of about 15%.
- Players under 900 minutes are pooled into a per-league replacement level; goalkeepers get defensive effects only. Two aliased defensive columns (Schmeichel, Leicester; Appiah, Caen) are ever-present players on teams that also have an ever-present goalkeeper.

Code: `research/R/identification.R`.

### Individual contributions

Model M3: league, home, score x 15-minute time bin, player counts, team attack and defense effects, plus ridge-penalized player offense and defense effects, on segments.

| Comparison                     | Held-out gain (SE)          |
|--------------------------------|-----------------------------|
| Teams over controls (M1 vs M0) | 1,374.2 (120.6)             |
| Players over teams (M3 vs M1)  | 135.9 (34.6), about 3.9 SEs |

- Cross-validated prior SD on player effects: 3.5% of shots per 90 (a typical player shifts his team's shot rate by about that much).
- Split-half reliability: 0.082 offense, 0.115 defense. Spearman-Brown implies about 0.2 for a full season.
- Team attack effects agree with the team-match model (correlation 0.982).
- **Likelihood:** Poisson. Splitting at goals pushes the Pearson dispersion to 1.47 (1.01 on stints) without real overdispersion; the Poisson likelihood over such a partition is the exact point-process likelihood, while a negative binomial reweights segments.
- **Superseded stint version:** 110 (SE 31) on stints, with the score stale after any goal inside a stint. archive: `reports/3_players.Rmd`.

Code: `research/R/player_models.R`.

### Structured priors and interactions

Every structured model was compared against M3 on the same folds.

| Model | Gain over M3 (SE) | Reliability, offense | Reliability, defense |
|------------------|------------------|------------------|------------------|
| M3, plain ridge | (reference) | 0.082 | 0.115 |
| Positional prior: position-group counts, unpenalized (M4) | 13.9 (21.0) | 0.090 | 0.117 |
| Teammate graph prior (kappa = 1) | 5.1 (5.9) | 0.101 | 0.110 |
| Role interactions (MRoles: M4 + 15 products of the counts) | -16.9 (17.0) | 0.090 | 0.114 |

- **None improves on plain ridge.** All gains are within about 1 SE.
- **Positional prior:** offensive position effects are real and mostly survive time controls. One more forward in place of a midfielder: +5.3% shots with score controls, +3.4% (SE 1.1) with score x time controls, so about a third of the forward effect was substitution timing. Centre backs -3.6% (SE 1.3), fullbacks -3.5% (SE 1.8). No defensive position effect is clearly nonzero (largest: forwards, +2.8%, SE 1.7). Goalkeeper counts are unidentified (nearly constant; an implausible +129%). archive: `reports/3b_positions.Rmd`.
- **Teammate graph prior:** penalty beta'(I + kappa L_norm) beta on offensive effects, L_norm the normalized Laplacian of shared minutes, fit by penalized IRLS (validated: kappa = 0 reproduces M3 within 0.1 to 0.2 deviance units). The prior SD rose from 0.034 to 0.042. Offensive reliability rose from 0.082 to 0.101, but within teams only from 0.077 to 0.083: the gain is teammates pulled toward their team mean, which the team effects already capture.
- **Role interactions:** -16.9 (SE 17.0). The 15 unpenalized products cost held-out deviance; an earlier implementation gave -2.0 (SE 22.7). Either way, nothing.
- The rebuilt teammate graph in `research/R/structure.R` reproduces the first implementation exactly (5.1, SE 5.9).

## B. Frequency ladder

The baseline refit on outcomes of increasing frequency (completed open-play passes and carries counted per segment by `research/build/build_events.py`).

| Outcome | Per team per 90 | Players over teams (SE) | Gain / SE | Prior SD | Reliability, offense | Reliability, defense |
|-----------|-----------|-----------|-----------|-----------|-----------|-----------|
| Goals | 1.1 | 43.3 (17.1) | 2.5 | 1.0% | 0.049 | -0.015 |
| Shots | 11.1 | 135.9 (34.6) | 3.9 | 3.5% | 0.082 | 0.115 |
| Box entries | 11.8 | 202.8 (42.7) | 4.8 | 3.5% | 0.098 | 0.051 |
| Final-third entries | 43.8 | 392.0 (96.4) | 4.1 | 2.8% | 0.146 | 0.110 |

- **No outcome reaches the go threshold of about 0.3.** Offense improves modestly with frequency (0.08 to 0.15, about 0.25 for a full season); defense does not improve. Reliability SEs are about 0.04.
- **Correction:** an earlier version of this file said frequency was not the main bottleneck, based on a simple noise calculation (reliability should have reached about 0.26 offense and 0.34 defense at four times the events). The simulation in Part D shows that calculation was wrong: on this design, reliability rises with frequency, but slowly at these rates, and the observed offensive values are what 3% effects produce. Defense on entries falls below the simulated curve, consistent with smaller defensive effects on entries.

## C. Interactions on final-third entries

The Part A structured models refit on final-third entries, same folds, against M3 on the same outcome.

| Model | Gain over M3 (SE) | Reliability, offense | Reliability, defense |
|------------------|------------------|------------------|------------------|
| M3, plain ridge | (reference) | 0.146 | 0.110 |
| M4, positional prior | 26.3 (15.7) | 0.152 | 0.114 |
| MRoles, role interactions | 0.9 (37.2) | 0.153 | 0.109 |
| MGraph, teammate graph | -6.5 (11.4) | 0.157 | 0.103 |

- **No interaction or structure beats ridge.** M4's 1.7 SEs is the largest gain of any structured model, but not significant; adding the 15 role interactions to it loses about 25 units.
- **Role coefficients look like noise:** 3 of 21 terms reach \|effect / SE\| of 2 or more, about what chance gives (roughly 1 expected), with zero held-out gain overall. With product terms present, the count coefficients are effects at zero of every other count and are not interpretable alone.
- **Not yet tested:** a cross-team similarity graph (players with similar action locations on different teams), the one version of a graph prior that can add information the team effects do not.

## D. Identifiability simulation

Player effects of known size (SD 3% and 6% per 90) planted on the real lineup design (actual segments, lineups, minutes, game states and team effects) at 1, 11, 44, 150 and 1,000 events per 90; outcomes simulated; M3 fit as in the real analysis, with the prior SD at its true value (the best case for ridge). Three replicates per setting; replicate-to-replicate SD of reliability is about 0.04. Code: `research/R/simulation.R`.

**Simulated split-half reliability, one season** (11 and 44 exact; other rates read from the figure):

| Events per 90 | True SD 3%, offense | True SD 3%, defense | True SD 6%, offense | True SD 6%, defense |
|---------------|---------------|---------------|---------------|---------------|
| 1 | about 0.02 | about 0.02 | about 0.05 | about 0.02 |
| 11 | 0.053 | 0.095 | about 0.15 | about 0.16 |
| 44 | 0.160 | 0.168 | about 0.33 | about 0.34 |
| 150 | about 0.31 | about 0.31 | about 0.52 | about 0.50 |
| 1,000 | about 0.60 | about 0.60 | about 0.77 | about 0.76 |

**Real against simulated (3%):**

| Outcome                      | Simulated | Real  |
|------------------------------|-----------|-------|
| Shots, offense               | 0.053     | 0.082 |
| Shots, defense               | 0.095     | 0.115 |
| Final-third entries, offense | 0.160     | 0.146 |
| Final-third entries, defense | 0.168     | 0.110 |

**More seasons (true SD 3%; same lineups repeated, so a floor).** Split-half reliability compares halves of the data, so with S seasons each half holds S/2 seasons; the full-data column applies Spearman-Brown, 2r / (1 + r). Accuracy correlates half-data estimates with the true effects.

| Side | Events per 90 | Seasons | Split-half reliability | Full-data reliability (approx.) | Accuracy |
|------------|------------|------------|------------|------------|------------|
| Offense | 11 | 1 | 0.053 | 0.10 | 0.186 |
| Offense | 11 | 2 | 0.142 | 0.25 | 0.290 |
| Offense | 11 | 4 | 0.213 | 0.35 | 0.346 |
| Offense | 44 | 1 | 0.160 | 0.28 | 0.346 |
| Offense | 44 | 2 | 0.322 | 0.49 | 0.448 |
| Offense | 44 | 4 | 0.431 | 0.60 | 0.556 |
| Defense | 11 | 1 | 0.095 | 0.17 | 0.209 |
| Defense | 11 | 2 | 0.165 | 0.28 | 0.245 |
| Defense | 11 | 4 | 0.211 | 0.35 | 0.341 |
| Defense | 44 | 1 | 0.168 | 0.29 | 0.318 |
| Defense | 44 | 2 | 0.340 | 0.51 | 0.441 |
| Defense | 44 | 4 | 0.473 | 0.64 | 0.561 |

- **The 3% curve reproduces the real results** on shots (both sides) and on final-third entries (offense) within replicate noise. The low reliability is what effects of that size give on these lineups, not a modeling failure.
- **Frequency matters, but the curve is shallow at real rates:** about 0.05 to 0.10 at 11 events per 90, 0.16 at 44, 0.31 at 150, 0.60 at 1,000. The value at 1,000 (nearly no counting noise) is the ceiling one season of these lineups allows.
- **Seasons help more than frequency at the same cost:** two seasons of entries double the split-half reliability (0.16 to 0.32 offense). Real seasons have roster turnover, which breaks teammate confounding, so these are floors.
- **Accuracy** (how close estimates get to the truth) is higher than reliability but still modest: 0.35 for one season of entries, 0.56 with four.

## Background results

### Team output: volume x quality

Team-match means: 11.8 open-play shots, 0.094 xG per shot, 1.11 xG.

- **Volume drives about 60% of team differences.** Elasticities of fitted xG: volume 0.61, quality 0.39 (sum 0.998). Same on Understat.
- **Volume:** negative binomial, mild overdispersion (deviance per df 1.08, same as Understat).
- **Quality is not gamma-distributed.** Moment and MLE shape estimates differ by nearly a factor of 2 (0.57 vs 1.02).
- **A single Tweedie model is contradicted.** The shot-level shape implies an index p of 1.64 against a fitted 1.49 (1.8 SEs; about 6 SEs on Understat). The two-part model (NB volume x gamma quality) predicts exactly as well (r = 0.534 for both) and is calibrated at both tails.
- **Three-component lognormal mixture on shot xG:** long-range cluster (about 4% of shots, median xG 0.007, about 34 yards), core (85.1%, median 0.048), high-value tail (10.7%, median 0.313, about 9 yards, converts 41.6%). The tail reproduces in every league; in half samples two solutions can tie, so the mixture is fit once on pooled data. Understat's sharp two-peak shape does not appear in StatsBomb.

archive: `R/models.R`, `R/diagnostics.R`, `reports/1_team_models.Rmd`. Earlier Understat analysis (EPL 2014/15 to 2024/25, 105,449 non-penalty shots, 8,360 team-match rows): archive: `data_pipeline.R`, `tweedie_fit.Rmd`, `nbg_fit.Rmd`, `analysis.Rmd`, `xg_what_im_fitting.md`.

### Outcome ladder toward quality (xG)

Same design and folds, five outcomes per segment row, plus the quality channel as its own model (context xG on segments with shots, offset log(shots)). Poisson for counts, quasi-Poisson for xG totals.

| Outcome | Players over teams (SEs) | Reliability, offense | Reliability, defense | Prior SD, true scale |
|---------------|---------------|---------------|---------------|---------------|
| Goals | 2.5 | 0.049 | -0.015 | 1.0% |
| Shots | 3.9 | 0.082 | 0.115 | 3.5% |
| Location xG (kernel) | 3.7 | 0.060 | 0.056 | 3.3% |
| Context xG (logistic, freeze frames) | 3.7 | 0.061 | 0.034 | 3.2% |
| StatsBomb xG | 3.0 | 0.068 | 0.046 | 4.1% |
| Quality per shot | 1.7 | 0.020 | 0.023 | 2.5% |

- **Shot counts carry the most player signal.** Goals are worst; xG-weighted outcomes predict about as well as shots but replicate worse.
- **Players affect volume, not quality.** Effects on quality per shot are small (SD 0.47% vs 1.11% for volume), unrelated to volume effects (correlation 0.008 offense, -0.051 defense), and do not replicate.
- Reliability SEs are about 0.04: the defensive gap and goals being worst are solid; offensive differences among shots and xG outcomes are within noise.
- Taking quality as "xG effect minus shot effect" forced a -0.84 correlation between channels through differential shrinkage; it is fit separately.
- glmnet assumes a dispersion of 1; an xG total's dispersion is E(q\^2)/E(q) (0.15 to 0.26), so prior SDs are reported on the true scale.

archive: `R/ladder.R`, `reports/4b_ladder.Rmd`.

### Shot quality from location and context

Same open-play shots, 10 held-out folds of whole matches.

| Model | Mean log loss | Share of StatsBomb's gain over a constant rate |
|------------------------|------------------------|------------------------|
| Constant goal rate | 0.3177 | 0% |
| Logistic: distance, distance\^2, angle | 0.2797 | 54% |
| Kernel xG, location only (bandwidth 1.5 yards) | 0.2776 | 57% |
| Kernel xG, location + freeze frames (0.7 SD) | 0.2566 | 87% |
| Logistic, location + freeze frames | 0.2532 | 92% |
| StatsBomb xG (not held out; trained elsewhere) | 0.2473 | 100% |

- **Location kernel xG** (Nadaraya-Watson, the Penn / WSABI Lecture 19 method) beats the parametric model by 149 deviance units (SE 21.8) and is calibrated (slope 1.05). The bandwidth is half the lecture's, consistent with four times as many shots.
- **What location misses is context.** Central shots 8 to 14 yards out all get about 0.14 from location while StatsBomb ranges from 0.1 to above 0.9. Freeze frames raise the correlation with StatsBomb there from 0.08 to 0.80 (kernel) and 0.83 (logistic).
- **Freeze-frame features:** goalkeeper distance and angle off the shot line, open goal share (opponents treated as a yard wide), nearest outfield defender, header flag. The keeper's angle correlates 0.58 with StatsBomb xG; median StatsBomb xG is about 0.48 when nearly the whole goal is open. Nearest defender is nearly uninformative. All open-play shots have a frame; the keeper is missing from 32.
- **With context, the parametric model wins:** the kernel with frames trails the logistic by 249 (SE 31.2) and under-predicts (3,289 kernel xG vs 3,453 goals), the curse of dimensionality.
- **Team level:** correlation with StatsBomb npxG rises from 0.66 (shot count) to 0.84 (location kernel total) and 0.94 (kernel with frames); with open-play goals from 0.28 to 0.38 and 0.53 (StatsBomb npxG: 0.58).

archive: `R/kernel_xg.R`, `R/freeze_features.R` (kernel version in the pre-prune state), `build/build_freeze.py`, `reports/4_kernel_xg.Rmd`, `reports/4a2_freeze_xg.Rmd`.

## Limits and open questions

- One season of these lineups cannot support individual ratings or pairwise synergies at any outcome frequency tried; the simulation quantifies how much more information would be needed (Part D).
- The simulation assumes player effects are stable within and across seasons and normally distributed; real effects may change with age, role or coach.
- **Next options:**
  1.  A higher-frequency outcome (about 150+ events per 90, such as completed passes into the attacking half or a possession-value measure on every action), buildable from the event files already downloaded; the simulation predicts split-half reliability of about 0.3 at 3% effects.
  2.  A multi-season extension on StatsBomb's free women's data (several FA WSL seasons with consistent player IDs), where roster turnover should beat the simulated floors.
  3.  A cross-team similarity graph, the one untested version of a graph prior.
