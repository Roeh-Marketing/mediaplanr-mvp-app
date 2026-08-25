nav_panel(
  title = "Plan",
  layout_sidebar(
    fillable = FALSE,

    # ------------------------------------------------------------------
    # Sidebar: source -> column mapping -> metadata -> build
    # ------------------------------------------------------------------
    sidebar = sidebar(
      width = 360,
      open  = TRUE,

      tags$h6("1. Source", class = "text-muted mt-1 mb-1"),
      fileInput("plan_upload", label = NULL,
                accept = c(".csv", ".tsv", ".xlsx", ".xls"),
                placeholder = "CSV or Excel...",
                buttonLabel = icon("upload")),
      div(class = "form-text mb-2 mt-n2",
          "An Excel workbook loads one scenario per sheet."),
      layout_columns(
        col_widths = c(6, 6),
        actionButton("plan_load_sample", "Weekly sample",
                     icon = icon("database"),
                     class = "btn-sm btn-outline-secondary w-100 mb-3"),
        actionButton("plan_load_flights", "Flight sample",
                     icon = icon("plane-departure"),
                     class = "btn-sm btn-outline-secondary w-100 mb-3")
      ),

      uiOutput("plan_source_status"),

      tags$h6("2. Columns", class = "text-muted mb-1"),
      radioButtons("map_mode", "The file has",
                   choices = c("One row per line item per week" = "weekly",
                               "One row per flight (in-market dates)" = "flights"),
                   selected = "weekly"),
      selectizeInput("map_grain", "Grain (what identifies a row)",
                     choices = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button", "drag_drop"),
                                    placeholder = "channel, partner, week...")),
      div(class = "form-text mb-2 mt-n2",
          "Order matters: coarsest first (channel, then partner)."),
      conditionalPanel(
        "input.map_mode == 'flights'",
        selectInput("map_flight_start", "Flight start column", choices = NULL),
        selectInput("map_flight_end", "Flight end column", choices = NULL),
        selectInput("map_week_start", "Weeks begin on",
                    choices = c("Monday", "Sunday", "Tuesday", "Wednesday",
                                "Thursday", "Friday", "Saturday")),
        div(class = "form-text mb-2 mt-n2",
            "Each buy is spread across its days and gathered into these weeks.")
      ),
      conditionalPanel(
        "input.map_mode != 'flights'",
        selectInput("map_week", "Week column", choices = NULL)
      ),
      selectInput("map_spend", "Planned spend column", choices = NULL),

      # Units are optional and most plans have none, so they stay folded away
      # until asked for. The package binds spend, units and rate by one
      # identity and computes whichever is missing, so mapping any two is
      # enough -- including leaving spend unmapped when units and a rate are
      # both present.
      accordion(
        open = FALSE,
        accordion_panel(
          "What it buys (optional)",
          icon = icon("chart-simple"),
          selectInput("map_unit_type", "Unit type column", choices = NULL),
          selectInput("map_units", "Planned units column", choices = NULL),
          selectInput("map_rate", "Rate column", choices = NULL),
          div(class = "form-text mt-n2",
              "Map any two of spend, units and rate; the third is computed. ",
              "Impressions are priced as a CPM, everything else per unit.")
        )
      ),

      uiOutput("plan_mapping_status"),

      tags$h6("3. Plan details", class = "text-muted mb-1 mt-3"),
      textInput("meta_name", "Name", placeholder = "Q2 2026 Media Plan"),
      textInput("meta_nickname", "Nickname", placeholder = "baseline"),
      layout_columns(
        col_widths = c(6, 6),
        textInput("meta_advertiser", "Advertiser", placeholder = "Acme Corp"),
        textInput("meta_planner", "Planner", placeholder = "your name")
      ),
      selectInput("meta_status", "Status",
                  choices = c("in development", "to review", "approved")),

      actionButton("plan_build", "Build plan", icon = icon("check"),
                   class = "btn-primary w-100 mt-2")
    ),

    # ------------------------------------------------------------------
    # Main
    # ------------------------------------------------------------------
    uiOutput("plan_kpis"),

    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE, height = "420px",
        card_header("Plan preview"),
        reactableOutput("plan_preview")
      ),
      card(
        full_screen = TRUE, height = "420px",
        card_header("Flighting"),
        plotOutput("plan_flight_plot", height = "100%")
      )
    ),

    card(
      card_header("Uploaded file"),
      card_body(
        class = "py-2",
        uiOutput("plan_file_note"),
        reactableOutput("plan_raw_preview")
      )
    )
  )
)
