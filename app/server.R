# ==============================================================================
# server.R
# Reactives chain the functions in R/. Anything that could live in a plain
# script belongs in R/, not here.
# ==============================================================================

server <- function(input, output, session) {

  # ---- data ------------------------------------------------------------------

  league_df <- reactive({
    req(input$league_select)
    load_league(input$league_select, LEAGUE_TABLE)
  })

  observeEvent(league_df(), {
    seasons <- season_choices(league_df())
    # Keep the current season if the new league also has it.
    keep <- if (isTruthy(input$season_select) && input$season_select %in% seasons) {
      input$season_select
    } else {
      seasons[1]
    }
    # Display "2021-22" in the picker while filtering on the raw "2021", so the
    # dropdown and the plot title name the campaign the same way.
    labelled <- stats::setNames(seasons, vapply(seasons, season_label, character(1)))
    updateSelectInput(session, "season_select", choices = labelled, selected = keep)
  })

  matches <- reactive({
    req(input$season_select)
    df <- league_df()
    req(input$season_select %in% df$Season)
    season_matches(df, input$season_select)
  })

  team_levels <- reactive(levels(matches()$ht))

  observeEvent(matches(), {
    teams <- team_levels()
    home_keep <- if (isTruthy(input$home_team) && input$home_team %in% teams) {
      input$home_team
    } else {
      teams[1]
    }
    away_keep <- if (isTruthy(input$away_team) && input$away_team %in% teams &&
                     input$away_team != home_keep) {
      input$away_team
    } else {
      setdiff(teams, home_keep)[1]
    }
    updateSelectInput(session, "home_team", choices = teams, selected = home_keep)
    updateSelectInput(session, "away_team", choices = teams, selected = away_keep)
  })

  # A team cannot host itself. Nudge the away side rather than rebuilding the
  # choice list, which would fight the user mid-selection.
  observeEvent(input$home_team, {
    req(input$home_team, input$away_team)
    if (identical(input$home_team, input$away_team)) {
      alt <- setdiff(team_levels(), input$home_team)[1]
      updateSelectInput(session, "away_team", selected = alt)
    }
  })

  # ---- model -----------------------------------------------------------------

  model_fit <- reactive({
    m <- matches()
    req(nrow(m) > 0)
    fit_double_poisson(m)
  })

  ratings <- reactive({
    extract_ratings(model_fit(), team_levels()) |>
      left_join(team_goal_diff(matches()), by = "team") |>
      mutate(team_label = shorten_team(team, input$league_select))
  })

  terms <- reactive(model_terms(model_fit()))

  # ---- quadrant map ----------------------------------------------------------

  # res = 96 rather than the 72dpi default. Point sizes are fixed, so raising
  # the resolution makes every label, axis title and annotation render larger
  # relative to the panel. This is the lever for plot text size, not base_size.
  output$quadrant_plot <- renderPlot(res = 96, {
    tm <- terms()
    plot_quadrant(
      ratings(),
      title = sprintf("%s (%s): team attack vs. defense ratings",
                      input$league_select, season_label(input$season_select)),
      subtitle = "Double Poisson GLM with sum-to-zero constraints (0 = league average)",
      caption = sprintf(
        "log(lambda) = mu + home + att - def  |  baseline %.2f goals, home advantage %.2fx",
        exp(tm$mu), tm$home_mult
      )
    )
  })

  # ---- matchup ---------------------------------------------------------------

  matchup <- reactive({
    req(input$home_team, input$away_team)
    req(input$home_team != input$away_team)
    r <- ratings()
    req(all(c(input$home_team, input$away_team) %in% r$team))

    lam <- match_lambdas(r, terms(), input$home_team, input$away_team)
    sm  <- score_matrix(lam$home, lam$away, max_goals = 8)

    list(
      lambda_h = lam$home,
      lambda_a = lam$away,
      score_mat = sm,
      outcomes = outcome_probs(sm),
      h2h = head_to_head(matches(), input$home_team, input$away_team)
    )
  })

  # The fixture the model is predicting: this season's leg at the selected
  # home ground, if the two teams met there.
  home_leg <- reactive({
    h <- matchup()$h2h
    h[h$venue == "Home leg", , drop = FALSE]
  })

  output$match_heatmap <- renderPlot(res = 96, {
    calc <- matchup()
    leg <- home_leg()
    actual <- if (nrow(leg) == 1) c(leg$home_goals[[1]], leg$away_goals[[1]]) else NULL

    plot_scoreline(
      calc$score_mat,
      home_name = input$home_team,
      away_name = input$away_team,
      display_max = 5,
      actual = actual
    )
  })

  output$odds_table <- renderTable({
    calc <- matchup()
    p <- calc$outcomes$prob
    tibble(
      Outcome = c(paste(input$home_team, "win"), "Draw",
                  paste(input$away_team, "win")),
      Probability = sprintf("%.1f%%", p * 100),
      `Fair decimal odds` = sprintf("%.2f", calc$outcomes$fair_odds)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Scoreboard strip above the prediction. Carries the fixture and both
  # expected-goal figures, so the tab opens on the answer.
  output$fixture_bar <- renderUI({
    calc <- matchup()
    div(
      class = "fixture-bar",
      div(class = "fixture-context",
          sprintf("%s  \u00b7  %s", input$league_select,
                  season_label(input$season_select))),
      div(class = "fixture-side",
          div(class = "fixture-team", input$home_team),
          div(class = "fixture-role", "Home")),
      div(class = "fixture-score",
          span(class = "xg", sprintf("%.2f", calc$lambda_h)),
          span(class = "xg-sep", "expected goals"),
          span(class = "xg", sprintf("%.2f", calc$lambda_a))),
      div(class = "fixture-side fixture-side--away",
          div(class = "fixture-team", input$away_team),
          div(class = "fixture-role", "Away"))
    )
  })

  # One-line version of the lambda ladder, sitting under the fixture bar. Uses
  # lambda_ladder() from docs.R, so the arithmetic here and the four-row table
  # on the documentation tab can never disagree.
  output$lambda_line <- renderUI({
    req(input$home_team, input$away_team)
    req(input$home_team != input$away_team)
    lad <- lambda_ladder(terms(), ratings(), input$home_team, input$away_team,
                         at_home = TRUE)

    parts <- lapply(seq_len(nrow(lad)), function(i) {
      r <- lad[i, ]
      tagList(
        if (i > 1) span(class = "lam-op", HTML("&times;")),
        span(class = "lam-term",
             span(class = "lam-num", sprintf("%.2f", r$factor)),
             span(class = "lam-lab", tolower(r$label)))
      )
    })

    div(
      class = "lambda-line",
      parts,
      span(class = "lam-op", "="),
      span(class = "lam-term lam-term--total",
           span(class = "lam-num", sprintf("%.2f", lad$running[nrow(lad)])),
           span(class = "lam-lab", "expected goals"))
    )
  })

  # Real-world comparison. Kept to a single compact panel below the prediction
  # so the predicted results stay the focus of the tab.
  output$h2h_panel <- renderUI({
    calc <- matchup()
    h2h <- calc$h2h

    if (nrow(h2h) == 0) {
      return(div(
        class = "panel-card panel-card--result",
        h5("What actually happened"),
        p(class = "muted", style = "margin-bottom: 0;",
          "These teams did not meet in this season's data.")
      ))
    }

    lines <- lapply(seq_len(nrow(h2h)), function(i) {
      row <- h2h[i, ]
      base <- sprintf("%s: %s %d-%d %s", row$venue, row$home_side,
                      row$home_goals, row$away_goals, row$away_side)

      # Only the home leg is on the same orientation as the heatmap, so only
      # that one gets the model's probability for the exact scoreline.
      if (row$venue == "Home leg") {
        sp <- scoreline_prob(calc$score_mat, row$home_goals, row$away_goals)
        detail <- if (is.na(sp$prob)) {
          " (scoreline outside the modelled grid)"
        } else {
          sprintf(" - model gave %.1f%%, the %s most likely scoreline",
                  sp$prob * 100, scales::ordinal(sp$rank))
        }
        tags$li(base, tags$span(detail, style = "color: #555;"))
      } else {
        tags$li(base)
      }
    })

    outcome_note <- {
      leg <- home_leg()
      if (nrow(leg) == 1) {
        actual_result <- if (leg$home_goals > leg$away_goals) "home"
                         else if (leg$home_goals < leg$away_goals) "away" else "draw"
        p_actual <- calc$outcomes$prob[calc$outcomes$result == actual_result]
        p(class = "muted", style = "margin: 0.6rem 0 0;",
          sprintf("The model put %.0f%% on that outcome before the match.",
                  p_actual * 100))
      } else {
        NULL
      }
    }

    div(
      class = "panel-card panel-card--result",
      h5("What actually happened"),
      tags$ul(lines),
      outcome_note
    )
  })

  # ---- tables and diagnostics ------------------------------------------------

  output$ratings_table <- renderTable({
    ratings() |>
      arrange(desc(overall)) |>
      transmute(
        Team = team,
        Attack = att, `Attack SE` = att_se,
        Defense = def, `Defense SE` = def_se,
        Overall = overall,
        # digits = 3 applies to every numeric column, which rendered counts as
        # "52.000". Format these two as text so they stay whole numbers.
        Played = sprintf("%d", as.integer(played)),
        `Goal diff` = sprintf("%+d", as.integer(gd))
      )
  }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$method_doc <- renderUI({
    req(input$home_team, input$away_team)
    req(input$home_team != input$away_team)
    m <- matches()
    method_doc(
      terms     = terms(),
      ratings   = ratings(),
      home_team = input$home_team,
      away_team = input$away_team,
      n_matches = nrow(m),
      n_teams   = length(team_levels())
    )
  })

  output$raw_summary <- renderPrint({
    summary(model_fit())
  })
}
