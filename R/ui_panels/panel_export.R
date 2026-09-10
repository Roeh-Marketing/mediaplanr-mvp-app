nav_panel(
  title = "Export",
  div(
    class = "container py-4",
    style = "max-width: 940px;",

    h4("Export", class = "mb-1"),
    p(class = "text-muted",
      "Excel or CSV. Every file carries the scenario label so the pieces can be ",
      "reassembled later."),

    card(
      class = "border-primary",
      card_header(icon("arrows-rotate"), " Workbook — one sheet per scenario"),
      card_body(
        p(class = "small text-muted mb-2",
          "Written in exactly the shape this app reads, so you can hand it to a ",
          "colleague, let them edit it in Excel, and upload it straight back — ",
          "each sheet returns as a scenario, with the sheet name as its nickname."),
        checkboxInput("exp_wb_selected_only",
                      "Only the scenarios ticked on the Review \u2192 Compare page", FALSE),
        uiOutput("exp_wb_note"),
        downloadButton("exp_workbook", "Download workbook (.xlsx)",
                       class = "btn-primary")
      )
    ),

    # The workbook is flat by construction -- a sheet is a table, and a table
    # cannot hold a tree. Once a plan has subplans, xlsx is a lossy export and
    # saying so here is better than letting someone discover it on re-upload.
    card(
      class = "mt-3",
      card_header(icon("code"), " JSON — the whole set, losslessly"),
      card_body(
        p(class = "small text-muted mb-2",
          "Everything the workbook cannot carry: subplan trees, lineage, ",
          "revisions and status, in the schema ", tags$code("mediaplanr"),
          " reads back with ", tags$code("plan_from_json()"), ". Use this ",
          "whenever a plan has detail plans hanging off it."),
        uiOutput("exp_json_note"),
        downloadButton("exp_json", "Download set (.json)",
                       class = "btn-outline-primary")
      )
    ),

    layout_columns(
      class = "mt-3",
      col_widths = c(6, 6),

      card(
        card_header("Comparison"),
        card_body(
          p(class = "small text-muted",
            "The summary table (one row per scenario) or the full cell-level ",
            "table across every scenario."),
          selectInput("exp_level", "Level",
                      choices = c("Summary — one row per scenario" = "summary",
                                  "Cells — line item x week x scenario" = "cell",
                                  "Flights — one buy x scenario" = "flight")),
          radioButtons("exp_cmp_fmt", "Format",
                       choices = c("Excel (.xlsx)" = "xlsx", "CSV" = "csv"),
                       selected = "xlsx", inline = TRUE),
          downloadButton("exp_compare", "Download comparison",
                         class = "btn-outline-primary w-100")
        )
      ),

      card(
        card_header("A single scenario"),
        card_body(
          p(class = "small text-muted",
            "One plan on its own, in the long shape it was loaded in."),
          selectInput("exp_scenario", "Scenario", choices = NULL),
          radioButtons("exp_plan_fmt", "Format",
                       choices = c("Excel (.xlsx)" = "xlsx", "CSV" = "csv"),
                       selected = "xlsx", inline = TRUE),
          downloadButton("exp_plan", "Download scenario",
                         class = "btn-outline-primary w-100")
        )
      )
    ),

    card(
      class = "mt-3",
      card_header("Everything, stacked"),
      card_body(
        p(class = "small text-muted mb-2",
          "All scenarios in one long table with a ", tags$code("scenario"),
          " column and its metadata — for pivoting elsewhere, rather than for ",
          "re-import."),
        downloadButton("exp_all", "Download stacked (.csv)",
                       class = "btn-outline-secondary")
      )
    ),

    uiOutput("exp_status")
  )
)
