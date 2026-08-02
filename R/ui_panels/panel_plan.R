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
      actionButton("plan_load_sample", "Load sample plan",
                   icon = icon("database"),
                   class = "btn-sm btn-outline-secondary w-100 mb-3"),

      uiOutput("plan_source_status"),

      tags$h6("2. Columns", class = "text-muted mb-1"),
      selectizeInput("map_grain", "Grain (what identifies a row)",
                     choices = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button", "drag_drop"),
                                    placeholder = "channel, partner, week...")),
      div(class = "form-text mb-2 mt-n2",
          "Order matters: coarsest first (channel, then partner)."),
      selectInput("map_week", "Week column", choices = NULL),
      selectInput("map_spend", "Planned spend column", choices = NULL),

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
