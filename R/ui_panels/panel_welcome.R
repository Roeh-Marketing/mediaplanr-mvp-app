nav_panel(
  title = "Welcome",
  div(
    class = "container py-4",
    style = "max-width: 980px;",

    h2("Media plan workbench", class = "mb-1"),
    p(class = "lead text-muted mb-4",
      "Bring a plan in, reshape it, fork scenarios off it, and see how they differ."),

    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("What this app does"),
        card_body(
          tags$ol(
            class = "mb-0 ps-3",
            tags$li(tags$b("Plan"), " — upload a CSV or Excel plan (or load the sample), ",
                    "map the columns, and build a validated plan."),
            tags$li(tags$b("Scenarios"), " — edit spend in a spreadsheet-style grid, ",
                    "with quick operations or by asking the assistant. ",
                    "Save the result as a named scenario."),
            tags$li(tags$b("Compare"), " — see the scenarios side by side as tables and charts."),
            tags$li(tags$b("Export"), " — download what you need.")
          )
        )
      ),
      card(
        card_header("Four ideas worth knowing"),
        card_body(
          tags$dl(
            class = "mb-0 small",
            tags$dt("A plan is intent"),
            tags$dd(class = "text-muted",
                    "Every row is ", tags$i("planned"), " spend — what you meant to buy, ",
                    "including for weeks already past. Actuals live elsewhere."),
            tags$dt("A line item"),
            tags$dd(class = "text-muted",
                    "A channel / partner combination. A row is a line item for one week."),
            tags$dt("A scenario is a fork"),
            tags$dd(class = "text-muted",
                    "Editing never changes the original. Each scenario records ",
                    "which plan it came from."),
            tags$dt("Status"),
            tags$dd(class = "mb-0 text-muted",
                    "in development → to review → approved. A scenario forked from an ",
                    "approved plan is never itself approved.")
          )
        )
      )
    ),

    div(
      class = "d-flex gap-2 mt-4 align-items-center",
      actionButton("welcome_load_sample", "Load the sample plan",
                   icon = icon("play"), class = "btn-primary"),
      actionButton("welcome_goto_plan", "Start from my own file",
                   icon = icon("upload"), class = "btn-outline-secondary"),
      uiOutput("welcome_hint", inline = TRUE)
    ),

    hr(class = "my-4"),
    p(class = "small text-muted mb-0",
      "Plan semantics come from the ", tags$code("mediaplanr"), " package. ",
      "This app does no forecasting or budget optimisation — that lives in the ",
      "companion ", tags$code("mrmopt"), " app.")
  )
)
