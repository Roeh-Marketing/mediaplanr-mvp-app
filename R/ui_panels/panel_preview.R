# ---------------------------------------------------------------------------
# Review -> Plan: one plan at a time, read only, plus the status control.
#
# Nothing here edits spend. This is the screen that shows a single plan whole --
# what it is, what it buys, when it runs -- which is why it is also where a plan
# is promoted through the review workflow. Approving something you cannot see is
# what the build-time status dropdown was asking people to do.
# ---------------------------------------------------------------------------

nav_panel(
  title = "Plan",

  div(
    class = "mp-actionbar d-flex flex-wrap align-items-end gap-3 mb-3",
    div(style = "min-width: 260px;",
        selectInput("pv_scenario", "Plan", choices = NULL, width = "100%")),
    div(style = "min-width: 190px;",
        selectInput("pv_status", "Status", choices = NULL, width = "100%")),
    div(class = "pb-3",
        actionButton("pv_status_apply", "Update status", icon = icon("check"),
                     class = "btn-primary")),
    div(class = "flex-grow-1 pb-3 text-end", uiOutput("pv_status_now", inline = TRUE))
  ),

  uiOutput("pv_kpis"),

  layout_columns(
    col_widths = c(5, 7),
    card(
      height = "360px",
      card_header("Plan details"),
      card_body(class = "pt-2", uiOutput("pv_meta"))
    ),
    card(
      full_screen = TRUE, height = "360px",
      card_header("Flighting"),
      plotOutput("pv_flight", height = "100%")
    )
  ),

  card(
    full_screen = TRUE,
    card_header("Plan grid"),
    card_body(
      class = "pt-2",
      div(class = "form-text mb-2",
          "Read only. Change spend on the ", tags$b("Edit"), " tab."),
      reactableOutput("pv_grid")
    )
  ),

  card(
    full_screen = TRUE,
    card_header("By line item"),
    card_body(class = "pt-2", reactableOutput("pv_items"))
  )
)
