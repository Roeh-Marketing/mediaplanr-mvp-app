nav_panel(
  title = "Compare",
  layout_sidebar(
    fillable = FALSE,

    sidebar = sidebar(
      width = 300,
      open  = TRUE,
      tags$h6("Scenarios", class = "text-muted mt-1 mb-1"),
      checkboxGroupInput("cmp_scenarios", label = NULL, choices = NULL),
      div(class = "d-flex gap-1 mb-2",
          actionButton("cmp_all", "All", class = "btn-sm btn-outline-secondary flex-fill"),
          actionButton("cmp_none", "None", class = "btn-sm btn-outline-secondary flex-fill")),
      hr(class = "my-2"),
      tags$h6("Delta chart", class = "text-muted mb-1"),
      selectInput("cmp_delta_scn", label = NULL, choices = NULL),
      div(class = "form-text mt-n2",
          "Which scenario to break down against the baseline."),
      hr(class = "my-2"),
      radioButtons("cmp_cell_view", "Cell table shows",
                   choices = c("Planned spend" = "spend",
                               "Delta vs base" = "delta",
                               "Share of total" = "share"),
                   selected = "spend")
    ),

    uiOutput("cmp_kpis"),

    card(
      full_screen = TRUE,
      card_header("Summary"),
      reactableOutput("cmp_summary")
    ),

    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE, height = "380px",
           card_header("Total spend"),
           plotOutput("cmp_plot_totals", height = "100%")),
      card(full_screen = TRUE, height = "380px",
           card_header("Mix"),
           plotOutput("cmp_plot_mix", height = "100%"))
    ),

    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE, height = "380px",
           card_header(
             "Flighting",
             div(class = "ms-auto",
                 selectInput("cmp_flight_basis", label = NULL,
                             choices = c("By week" = "week", "By month" = "month",
                                         "By day" = "day"),
                             selected = "week", width = "130px"))
           ),
           plotOutput("cmp_plot_flight", height = "100%")),
      card(full_screen = TRUE, height = "380px",
           card_header("What changed"),
           plotOutput("cmp_plot_deltas", height = "100%"))
    ),

    card(
      full_screen = TRUE,
      card_header("By line item and week"),
      card_body(class = "pt-2", reactableOutput("cmp_cells"))
    )
  )
)
