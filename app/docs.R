# ==============================================================================
# docs.R  -- the "How the model works" panel
#
# Built from the fitted model rather than written as static prose, so every
# number in the explanation belongs to the season currently on screen.
#
# Math is typeset with MathJax via shiny::withMathJax(). Two escaping rules:
#   * backslashes double in R strings: "\\lambda" produces \lambda
#   * MathJax uses \\( ... \\) for inline and $$ ... $$ for display.
#     A bare $ is NOT an inline delimiter in this configuration.
# MathJax loads from a CDN, so offline the formulas appear as raw TeX. The
# ladders and prose carry the explanation on their own if that happens.
# ==============================================================================

#' Wrap a display equation so it can be wider than the prose column and
#' scroll horizontally instead of clipping on narrow windows.
eq <- function(...) shiny::tags$div(class = "eq", ...)

#' Turn a log-scale coefficient into the multiplier a reader can act on.
as_multiplier <- function(x) sprintf("%.2fx", exp(x))

#' Team names go inside \text{}, so strip anything TeX would treat as markup.
tex_safe <- function(x) gsub("[^A-Za-z0-9 .'-]", "", as.character(x))

#' The build-up of one side's expected goals, as a sequence of multiplications.
#'
#' Returns a tibble with one row per model term: the factor it contributes and
#' the running total after applying it. The last running value is exactly the
#' lambda used everywhere else in the app.
#'
#' `ratings$def` is stored already negated (positive = concedes fewer), so the
#' opponent's defence enters as exp(-def): a leaky opponent pushes the total up.
lambda_ladder <- function(terms, ratings, attacking_team, defending_team,
                          at_home = TRUE) {
  pick <- function(team, column) ratings[[column]][ratings$team == team]

  att <- pick(attacking_team, "att")
  def <- pick(defending_team, "def")

  steps <- tibble::tibble(
    label  = c("League baseline",
               "Home advantage",
               sprintf("%s attack", attacking_team),
               sprintf("%s defense", defending_team)),
    symbol = c("e^{\\mu}", "e^{\\gamma}", "e^{\\alpha_i}", "e^{-\\delta_j}"),
    factor = c(exp(terms$mu),
               exp(terms$home_adv),
               exp(att),
               exp(-def)),
    note   = c("average goals per side, per match",
               "the same for every team in the league",
               sprintf("rating %+.3f on the log scale", att),
               sprintf("rating %+.3f on the log scale", def))
  )

  if (!at_home) steps <- steps[-2, ]

  dplyr::mutate(steps, running = cumprod(factor))
}

#' Render one ladder as an HTML table. The symbol column ties each row back to
#' the term it represents in the displayed equation.
ladder_table <- function(ladder, heading) {
  rows <- lapply(seq_len(nrow(ladder)), function(i) {
    r <- ladder[i, ]
    shiny::tags$tr(
      shiny::tags$td(
        class = "lad-label",
        r$label,
        shiny::tags$span(class = "lad-note", r$note)
      ),
      shiny::tags$td(
        class = "lad-sym",
        shiny::HTML(sprintf("\\(%s\\)", r$symbol))
      ),
      shiny::tags$td(
        class = "lad-factor",
        if (i == 1) "" else sprintf("x %.2f", r$factor)
      ),
      shiny::tags$td(class = "lad-run", sprintf("%.2f", r$running))
    )
  })

  shiny::tags$div(
    class = "ladder",
    shiny::tags$div(class = "lad-head", heading),
    shiny::tags$table(shiny::tags$tbody(rows)),
    shiny::tags$div(
      class = "lad-foot",
      sprintf("Expected goals: %.2f", ladder$running[nrow(ladder)])
    )
  )
}

#' The full documentation panel.
method_doc <- function(terms, ratings, home_team, away_team, n_matches, n_teams) {
  home_ladder <- lambda_ladder(terms, ratings, home_team, away_team, at_home = TRUE)
  away_ladder <- lambda_ladder(terms, ratings, away_team, home_team, at_home = FALSE)

  # The same arithmetic as the home ladder, written as an equation.
  hf <- home_ladder$factor
  numeric_line <- sprintf(
    "$$\\lambda_{\\text{%s}} = %.2f \\times %.2f \\times %.2f \\times %.2f = %.2f$$",
    tex_safe(home_team), hf[1], hf[2], hf[3], hf[4],
    home_ladder$running[nrow(home_ladder)]
  )

  shiny::withMathJax(shiny::tagList(

    # ---- 1. the assumption ---------------------------------------------------
    shiny::tags$div(
      class = "doc-block",
      shiny::tags$h3(class = "doc-h", "What the model assumes"),
      shiny::tags$p(
        "Every team gets two numbers: an attack rating ",
        shiny::HTML("\\(\\alpha_i\\)"), " and a defense rating ",
        shiny::HTML("\\(\\delta_i\\)"), ". The goals a side scores are treated ",
        "as a draw from a Poisson distribution with mean ",
        shiny::HTML("\\(\\lambda\\)"), ", which depends on that side's attack, ",
        "the opponent's defense, and whether the side is at home. Two draws per ",
        "match, one for each side, which is why this is called a ",
        shiny::tags$em("double"), " Poisson model."
      ),
      shiny::tags$p(
        class = "doc-fit",
        sprintf("Fitted on %s matches between %s teams in the selected season.",
                format(n_matches, big.mark = ","), n_teams)
      )
    ),

    # ---- 2. the equation, as multiplication ----------------------------------
    shiny::tags$div(
      class = "doc-block doc-wide",
      shiny::tags$h3(class = "doc-h", "Expected goals is a product"),
      shiny::tags$p(
        "Start from the league average and multiply once for each thing that ",
        "makes this fixture different from an average one:"
      ),
      eq(shiny::HTML(
        "$$\\lambda_{\\text{home}} =
           \\underbrace{e^{\\mu}}_{\\text{baseline}} \\times
           \\underbrace{e^{\\gamma}}_{\\text{home}} \\times
           \\underbrace{e^{\\alpha_i}}_{\\text{attack}} \\times
           \\underbrace{e^{-\\delta_j}}_{\\text{defense}}$$"
      )),
      shiny::tags$p(
        "With the teams currently selected, that product is:"
      ),
      eq(shiny::HTML(numeric_line)),
      shiny::tags$div(
        class = "ladder-row",
        ladder_table(home_ladder, sprintf("%s at home", home_team)),
        ladder_table(away_ladder, sprintf("%s away", away_team))
      ),
      shiny::tags$p(
        class = "doc-note",
        "The away side has no ", shiny::HTML("\\(e^{\\gamma}\\)"), " row. That ",
        "missing row is what home advantage is worth: ",
        sprintf("%s on goals.", as_multiplier(terms$home_adv))
      )
    ),

    # ---- 3. why logs ---------------------------------------------------------
    shiny::tags$div(
      class = "doc-block",
      shiny::tags$h3(class = "doc-h", "Why the ratings look like small decimals"),
      shiny::tags$p(
        "Products are awkward to fit, so the model works on the log scale, ",
        "where multiplying becomes adding:"
      ),
      eq(shiny::HTML(
        "$$\\log \\lambda_{\\text{home}} = \\mu + \\gamma + \\alpha_i - \\delta_j$$"
      )),
      shiny::tags$p(
        "The two equations are the same statement, because ",
        shiny::HTML("\\(e^{a+b} = e^{a} \\cdot e^{b}\\)"), ". That is the entire ",
        "reason the ratings are numbers like 0.26 rather than multipliers like 1.30."
      ),
      shiny::tags$table(
        class = "term-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th("Rating"), shiny::tags$th("Means"), shiny::tags$th("On goals")
        )),
        shiny::tags$tbody(
          shiny::tags$tr(
            shiny::tags$td("+0.25"), shiny::tags$td("clearly above average"),
            shiny::tags$td(as_multiplier(0.25))
          ),
          shiny::tags$tr(
            shiny::tags$td("0.00"), shiny::tags$td("exactly league average"),
            shiny::tags$td("1.00x")
          ),
          shiny::tags$tr(
            shiny::tags$td("-0.25"), shiny::tags$td("clearly below average"),
            shiny::tags$td(as_multiplier(-0.25))
          )
        )
      ),
      shiny::tags$p(
        "Zero is league average by construction, not by coincidence. The ratings ",
        "are forced to sum to zero across the league:"
      ),
      eq(shiny::HTML(
        "$$\\sum_{i=1}^{k} \\alpha_i = 0 \\qquad \\sum_{i=1}^{k} \\delta_i = 0$$"
      )),
      shiny::tags$p(
        class = "doc-note",
        "Without that constraint the model is unidentifiable: add 1 to every ",
        "attack rating and subtract 1 from every defense rating and the ",
        "predictions are identical."
      )
    ),

    # ---- 4. from goals to probabilities --------------------------------------
    shiny::tags$div(
      class = "doc-block",
      shiny::tags$h3(class = "doc-h", "From expected goals to a scoreline"),
      shiny::tags$p(
        sprintf("Expected goals is an average, not a prediction: %.2f does not mean %s ",
                home_ladder$running[nrow(home_ladder)], home_team),
        "will score that many. The Poisson distribution turns the average into a ",
        "probability for each possible number of goals:"
      ),
      eq(shiny::HTML(
        "$$P(Y = y) = \\frac{\\lambda^{y} e^{-\\lambda}}{y!}$$"
      )),
      shiny::tags$p(
        "Multiplying the two sides' probabilities gives every scoreline, and ",
        "summing the cells on each side of the diagonal gives the match odds:"
      ),
      eq(shiny::HTML(
        "$$P(\\text{home win}) = \\sum_{a > b} P(Y_h = a)\\, P(Y_a = b)$$"
      )),
      shiny::tags$p(
        class = "doc-note",
        "That multiplication assumes the two scores are independent. They are ",
        "not quite: real matches produce slightly more draws and more 0-0s than ",
        "this model expects. The Dixon-Coles adjustment exists to correct exactly that."
      )
    ),

    # ---- 5. the full specification -------------------------------------------
    shiny::tags$details(
      class = "doc-details",
      shiny::tags$summary("The full specification"),
      shiny::tags$div(
        class = "doc-details-body",
        eq(shiny::HTML(
          "$$\\begin{aligned}
             Y_{h} &\\sim \\text{Poisson}(\\lambda_{h}), \\quad
             Y_{a} \\sim \\text{Poisson}(\\lambda_{a}) \\\\[4pt]
             \\log \\lambda_{h} &= \\mu + \\gamma + \\alpha_i - \\delta_j \\\\[2pt]
             \\log \\lambda_{a} &= \\mu + \\alpha_j - \\delta_i
           \\end{aligned}$$"
        )),
        shiny::tags$p(
          shiny::HTML("\\(\\mu\\)"), " is the log of the league's average goals ",
          "per side. ", shiny::HTML("\\(\\gamma\\)"), " is home advantage, shared ",
          "by every team. Index ", shiny::HTML("\\(i\\)"), " is the home side and ",
          shiny::HTML("\\(j\\)"), " the away side."
        ),
        shiny::tags$p(
          sprintf("This season: \\(\\mu = %+.3f\\), so the baseline is %.2f goals. ",
                  terms$mu, exp(terms$mu)) |> shiny::HTML(),
          sprintf("\\(\\gamma = %+.3f\\), so home advantage is %s (p = %.3g).",
                  terms$home_adv, as_multiplier(terms$home_adv),
                  terms$home_p_value) |> shiny::HTML()
        ),
        shiny::tags$p(
          "The sum-to-zero constraint means only ", shiny::HTML("\\(k-1\\)"),
          " ratings are estimated per block. The last one is recovered from the rest:"
        ),
        eq(shiny::HTML(
          "$$\\alpha_k = -\\sum_{i=1}^{k-1} \\alpha_i$$"
        )),
        shiny::tags$p(
          class = "doc-note",
          "Its standard error follows from the delta method: for a linear ",
          "combination with weights all equal to -1, the variance is the sum of ",
          "every entry of that block's covariance submatrix."
        ),
        shiny::tags$p(
          class = "doc-note",
          "Defense enters with a minus sign, so a larger ",
          shiny::HTML("\\(\\delta\\)"), " lowers the opponent's expected goals. ",
          "The app negates the raw coefficient before display, so that on both ",
          "axes of the quadrant map, higher is better."
        )
      )
    )
  ))
}
