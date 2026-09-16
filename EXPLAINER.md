---
editor_options: 
  markdown: 
    wrap: 72
---

# Explaining the app

A reference for presenting the double Poisson soccer module. Sections 1
to 3 are the core pitch. Sections 4 to 6 are the mechanism. Sections 7
to 12 are what to say when questioned.

------------------------------------------------------------------------

## 1. What the app is, in one breath

It takes a season of football results and works out how strong each
team's attack and defense were, then uses those strengths to give a
probability for every possible scoreline in any matchup you pick.

If someone wants more: the only input is who played whom and what the
score was. Everything else is calculated.

------------------------------------------------------------------------

## 2. The input data

Five columns, nothing more:

| Season | Home team | Away team  | Home goals | Away goals |
|--------|-----------|------------|------------|------------|
| 2021   | AC Milan  | AC Venezia | 2          | 0          |

No possession, no shots, no player data, no betting odds. This matters
because it sets up the surprising part: from that alone you can extract
meaningful team strengths.

Data comes from the `footBayes` R package, the companion package to the
book this module follows.

------------------------------------------------------------------------

## 3. The two numbers per team

Each team gets an **attack rating** and a **defense rating**.

-   Attack: how many goals this team scores relative to an average team
-   Defense: how many goals this team concedes relative to an average
    team
-   **0 means exactly league average.** Positive is better on both.

Twenty teams gives 40 ratings, plus one **home advantage** shared by the
whole league. 41 numbers summarizing 380 matches.

**The key claim:** these ratings are not in the data. They are
estimated. Nothing in the dataset is labeled "Milan's attack." The model
infers them.

------------------------------------------------------------------------

## 4. How the ratings get calculated

### Step 1: Reshape each match into two rows

The raw data has one row per match. The model needs one row per *scoring
performance*, because each match contains two.

Milan 2-0 Venezia becomes:

| Goals | At home? | Attacking | Defending |
|-------|----------|-----------|-----------|
| 2     | yes      | Milan     | Venezia   |
| 0     | no       | Venezia   | Milan     |

380 matches become **760 rows**.

**Why this reshape is the whole trick.** In the original format, "Milan"
appears in a home column and an away column, and a regression has no way
to treat those as the same team in the same role. After stacking, every
team appears in one `attacking` column and one `defending` column, 38
times in each across the season. That is what lets a single model
estimate both of Milan's ratings.

### Step 2: What the regression solves

The model proposes that every row's goal count follows:

```         
expected goals = baseline x home advantage x attacker's strength x defender's leakiness
```

For each of the 760 rows, plug in the relevant ratings to get a
predicted average. The Poisson then says how likely the *actual*
observed count was, given that average.

**Maximum likelihood, plainly:** try a set of 41 numbers, compute how
probable the real 760 goal counts would be under them, adjust, repeat.
The winner is whichever set makes the observed season most probable.
`glm()` does this in a fraction of a second.

The 41 numbers: 1 baseline, 1 home advantage, 20 attack ratings, 20
defense ratings. The sum-to-zero constraint removes one degree of
freedom per block, so R estimates 19 attack and 19 defense contrasts and
recovers the rest.

### Step 3: Every match teaches the model four things at once

One match constrains four ratings: Milan's attack (scored 2), Venezia's
defense (conceded 2), Venezia's attack (scored 0), Milan's defense
(conceded 0).

Across the season every rating is pinned down by 38 matches, and every
match pulls on four ratings. The parameters are interlocked, which is
why this must be one simultaneous regression rather than 20 separate
per-team calculations.

**This is also where opponent adjustment comes from.** If Milan score 3
against Salernitana, that match pushes Milan's attack up *and*
Salernitana's defense down. When another team also scores 3 against
Salernitana, that now-low defense rating means less credit gets
assigned. Strength of schedule falls out of the joint fitting
automatically. Nobody wrote an adjustment for it.

### Step 4: Recovering the twentieth team

R estimates only 19 attack ratings directly. The last comes from the
constraint:

```         
20th team's rating = -(sum of the other 19)
```

With the sum-to-zero rule, knowing 19 determines the 20th exactly.
Estimating all 20 would be redundant and the regression would refuse to
run. Which team gets left out is whichever falls last alphabetically,
and it carries no meaning. `extract_ratings()` recovers it and computes
its standard error by the delta method, so it is not second-class in the
output.

### Step 5: The sign flip on defense

The raw coefficient for defense means "concedes more," so a high raw
value is *leaky*. That is backwards for a chart where up-and-right
should be good, so the code negates it. On screen, **positive defense
means concedes fewer goals.**

Worth knowing because the Model summary tab shows raw, un-flipped
coefficients. If someone compares tabs and finds the signs reversed,
this is why.

### Step 6: What comes out

```         
Team          Attack   Defense   Overall
Inter          0.458     0.466     0.924
Napoli         0.330     0.507     0.837
Milan          0.260     0.512     0.772
```

Exponentiate to interpret: Inter's 0.458 means about **1.58x** the
league average, adjusted for schedule and venue.

**The standard errors matter.** Attack SEs run about 0.11 to 0.15. Two
teams within roughly 0.2 of each other are not meaningfully
distinguishable from 38 matches. That is the honest counterweight to
reading the quadrant map as a definitive ranking.

------------------------------------------------------------------------

## 5. Why the ratings beat raw goals scored

The best point to make if you want to show the model earns its
complexity.

Raw goals scored treats every goal equally. The model does not, because
it knows the fixture list. Three against the worst team at home counts
for less than three against the champions away.

**Where to point on screen:** the quadrant map positions each dot by
*model rating* and colors it by *actual goal difference*. Where those
disagree, the model is saying the raw numbers flatter or understate that
team once you account for who they played.

------------------------------------------------------------------------

## 6. Why the ratings look like 0.26 instead of 1.30

The model works with logarithms, because that turns multiplication into
addition, and addition is what a regression can fit. Undo the log to get
the meaningful number:

| Rating | Multiplier on goals    |
|--------|------------------------|
| +0.25  | 1.28x                  |
| 0.00   | 1.00x, exactly average |
| -0.25  | 0.78x                  |

**One-liner:** ratings are on the log scale, so exponentiate to get a
multiplier.

### Why 0 means league average

Team strengths only ever appear as differences, attack minus defense.
You could add 1 to every attack rating and subtract 1 from every defense
rating and get identical predictions, so without a rule there are
infinitely many equally valid answers.

The fix is requiring all attack ratings to sum to zero, same for
defense. That picks one specific answer, and it is the one where zero
means average. In R that setting is `contr.sum`.

------------------------------------------------------------------------

## 7. From ratings to expected goals

For a specific fixture, multiply four things:

```         
1.26  x  1.10  x  1.30  x  1.29  =  2.32
```

| Factor | What it is                                        |
|--------|---------------------------------------------------|
| 1.26   | league baseline, average goals per side per match |
| 1.10   | home advantage, same for every team               |
| 1.30   | Milan's attack, 30% above average                 |
| 1.29   | Venezia's defense, concedes 29% above average     |

Milan are expected to score **2.32** goals. For the away side, drop the
home advantage factor. That missing 1.10 *is* home advantage, which is a
clean way to show what it is worth.

This ladder appears under the fixture bar and in full on the explanation
tab.

------------------------------------------------------------------------

## 8. Expected goals is not a prediction

The most common misunderstanding. 2.32 is an average. Milan will never
score 2.32 goals. It is what they would average if the fixture were
replayed many times.

To get from an average to actual probabilities you need a distribution.
That is the Poisson.

------------------------------------------------------------------------

## 9. What the Poisson does

The Poisson is the standard tool for counting things that happen at a
steady average rate: goals, arrivals, typos per page. Feed it an average
and it returns a probability for every possible count.

With an average of 2.32:

| Goals | Probability |
|-------|-------------|
| 0     | 9.8%        |
| 1     | 22.8%       |
| 2     | 26.5%       |
| 3     | 20.5%       |

The most likely single outcome is only 26.5%. **Football is mostly
unpredictable, and the model says so.**

Worth mentioning: you never estimate the spread separately. Once you
specify the average, the Poisson's spread follows automatically. That is
why the model produces a probability grid with no extra machinery.

------------------------------------------------------------------------

## 10. Why "double" Poisson, and how the grid works

Two Poisson distributions per match, one per side. Poisson 1 governs the
home side's goals with mean 2.32. Poisson 2 governs the away side's with
mean 0.50.

**Nuance if pressed:** these are not "one per team" in the sense of
belonging to a team. Each depends on *both* teams. Milan's 2.32 comes
from Milan's attack and Venezia's defense. Change the opponent and it
changes.

Multiply the two to get any cell:

```         
P(Milan 2, Venezia 0)  =  0.265 x 0.607  =  16.1%
```

Do that for every combination to get the heatmap, then sum: cells where
home \> away give the home win probability, the diagonal gives the draw,
the rest gives the away win. **Milan 78.5%, draw 15.1%, Venezia 6.3%.**

------------------------------------------------------------------------

## 11. The two honest caveats

### It is fitted and tested on the same season

Milan 2-0 Venezia was one of the 380 matches used to calculate the
ratings. So when the app says the model gave that result 16.1%, that is
not a forecast. The result helped set the parameters.

**Why it is still fine:** the app exists to show what the parameters
mean and how they combine, and it does that honestly. The ratings really
do describe the season correctly.

**Where it would stop being fine:** attaching the word "accuracy" to any
number. That is why the README says fit quality instead.

**The fix if asked:** walk-forward validation. Fit on the first ten
matchweeks, predict week eleven, refit, predict twelve, and so on, so
every prediction uses only prior information. Not implemented.

### It assumes the two scores are independent

Multiplying the two Poissons assumes how many Milan score tells you
nothing about how many Venezia score. Not quite true: real matches
produce slightly more draws and more 0-0s, probably because teams
respond to game state.

**The known fix:** the Dixon-Coles adjustment from a 1997 paper, which
nudges the probabilities for low-scoring results. Not implemented, and
the obvious next extension.

------------------------------------------------------------------------

## 12. Tab by tab

| Tab | Shows | Point being made |
|------------------------|------------------------|------------------------|
| Quadrant map | All 20 teams on attack vs defense | Season compressed to two numbers per team; color vs position shows model rating disagreeing with raw goal difference |
| Match predictor | Scoreline grid, odds, actual result | Expected goals is a distribution; even the top scoreline is only 16% |
| Team ratings | Exact numbers with standard errors | Uncertainty is real; mid-table teams sit within each other's error bars |
| How it works | The math with live numbers | Log scale, the multiplication ladder, the sum-to-zero constraint |
| Model summary | Raw `glm` output | It is all one regression, no sport-specific machinery |

------------------------------------------------------------------------

## 13. Likely questions

**Is it accurate?** Cannot say from this app, because it is tested on
its own training data. It describes the season well. Measuring
forecasting accuracy needs walk-forward validation.

**Could this beat a bookmaker?** No. Bookmakers use squads, injuries,
transfers, lineups, in-play information and money flow. This uses final
scores only.

**Why Poisson and not something else?** Goals are counts arriving at a
roughly steady rate, and football scores fit Poisson closely in
practice. Standard since Maher's 1982 paper.

**What if a team plays only part of a season?** The fit breaks, and the
app stops with an explanatory message rather than returning wrong
numbers.

**Does it account for injuries, transfers, form?** No. One rating per
team for the whole season, no time weighting, no carryover between
seasons.

**Where is this from?** Egidi, Karlis and Ntzoufras, *Predictive
Modelling for Football Analytics*, Chapman & Hall/CRC, 2025. `footBayes`
is its companion package.

**Does this work for other sports?** The structure fits any
repeated-encounters setting where you want per-entity strengths: chess
ratings, A/B tests, the NBA and NHL modules. Poisson specifically suits
low-scoring count sports, so hockey transfers well and basketball would
need a different distribution.

------------------------------------------------------------------------

## 14. If you only remember five things

1.  Input is just scorelines. Ratings are calculated, not given.
2.  One regression estimates 41 numbers from 760 rows, and every match
    constrains four ratings at once.
3.  Ratings adjust for opponent and venue, which raw goal difference
    does not.
4.  The Poisson turns an average into a probability for every scoreline.
    Even the most likely one is usually under 20%.
5.  It is fitted and tested on the same season, so it demonstrates
    meaning, not accuracy.
