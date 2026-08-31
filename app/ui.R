# ==============================================================================
# ui.R
# ==============================================================================

ui <- fluidPage(
  theme = app_theme(),
  tags$head(tags$style(HTML(app_css()))),

  div(
    class = "app-header",
    h1("Predictive modeling for football analytics"),
    p("Double Poisson GLM with sum-to-zero constraints")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "sidebar-panel",

      p(class = "eyebrow", "Dataset"),
      selectInput("league_select", "League", choices = LEAGUE_NAMES,
                  selected = LEAGUE_NAMES[1]),
      selectInput("season_select", "Season", choices = character(0)),

      hr(),

      p(class = "eyebrow", "Matchup"),
      selectInput("home_team", "Home team", choices = character(0)),
      div(class = "vs-rule", span("vs")),
      selectInput("away_team", "Away team", choices = character(0)),

      hr(),

      helpText(
        "Identifiability is resolved with ", code("contr.sum"),
        ", so 0 is the league average. Ratings are fitted on one full season ",
        "at a time."
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Match outcome predictor",
          br(),
          uiOutput("fixture_bar"),
          # uiOutput("lambda_line"),
          fluidRow(
            column(
              6,
              p(class = "section-title", "Scoreline probabilities"),
              div(class = "plot-scroll",
                  plotOutput("match_heatmap", height = "430px"))
            ),
            column(
              6,
              p(class = "section-title", "Outcome odds"),
              tableOutput("odds_table"),
              uiOutput("h2h_panel")
            )
          )
        ),
        tabPanel(
          "How the model works",
          br(),
          uiOutput("method_doc")
        ),
        tabPanel(
          "League quadrant map",
          br(),
          div(class = "plot-scroll",
              plotOutput("quadrant_plot", height = "620px"))
        ),
        tabPanel(
          "Team ratings",
          br(),
          tableOutput("ratings_table")
        ),
        tabPanel(
          "Model summary",
          br(),
          verbatimTextOutput("raw_summary")
        )
      )
    )
  )
)
