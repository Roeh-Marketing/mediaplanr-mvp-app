# ---------------------------------------------------------------------------
# Review -> Structure: how the plans relate to each other.
#
# The other two Review subtabs answer "what is in this plan" and "how do these
# plans differ". This one answers the two shape questions neither of them can:
#
#   * Where did each scenario come from? Compare measures everything against
#     the baseline; LINEAGE measures each scenario against the plan it was
#     actually derived from, which is the only view where a chain -- base, a
#     scenario off it, a scenario off THAT -- reads as a chain rather than as
#     three things beside each other.
#   * What hangs beneath this plan? A topline whose cells are backed by
#     subplans owned elsewhere.
#
# Everything here is drawn by mediaplanr.viz from projections mediaplanr owns
# (lineage(), subplan_map()), so the app never rebuilds a layout by hand and
# then drifts from the package that defines it.
# ---------------------------------------------------------------------------

nav_panel(
  title = "Structure",

  div(
    class = "mp-actionbar d-flex flex-wrap align-items-end gap-3 mb-3",
    div(style = "min-width: 260px;",
        selectInput("str_scenario", "Plan", choices = NULL, width = "100%")),
    div(style = "min-width: 240px;",
        selectInput("str_cell", "Cell", choices = NULL, width = "100%")),
    div(class = "pb-3", uiOutput("str_cell_action", inline = TRUE)),
    div(class = "flex-grow-1 pb-3 small text-muted",
        uiOutput("str_note", inline = TRUE))
  ),

  card(
    full_screen = TRUE, height = "420px",
    card_header(
      div(class = "d-flex justify-content-between align-items-center",
          span(icon("code-branch"), " Where each scenario came from"),
          span(class = "small text-muted",
               "Edges show the change against the parent, not the baseline"))
    ),
    card_body(class = "pt-2", echarts4r::echarts4rOutput("str_lineage", height = "100%"))
  ),

  layout_columns(
    col_widths = c(7, 5),

    card(
      full_screen = TRUE, height = "400px",
      card_header(icon("sitemap"), " What hangs beneath this plan"),
      card_body(class = "pt-2", echarts4r::echarts4rOutput("str_tree", height = "100%"))
    ),

    card(
      full_screen = TRUE, height = "400px",
      card_header(icon("chart-pie"), " Where the money sits"),
      card_body(class = "pt-2", echarts4r::echarts4rOutput("str_sunburst", height = "100%"))
    )
  ),

  card(
    full_screen = TRUE,
    card_header(icon("table-list"), " Every plan in the tree"),
    card_body(class = "pt-2", reactableOutput("str_table"))
  ),

  # Only meaningful once something is attached, so the server hides them until
  # then rather than showing two empty frames captioned "nothing here".
  uiOutput("str_owned")
)
