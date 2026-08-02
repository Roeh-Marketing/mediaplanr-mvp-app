nav_panel(
  title = "Scenarios",
  layout_sidebar(
    fillable = FALSE,

    # ------------------------------------------------------------------
    # Sidebar: which scenario, quick operations, save
    # ------------------------------------------------------------------
    sidebar = sidebar(
      width = 340,
      open  = TRUE,

      tags$h6("Editing", class = "text-muted mt-1 mb-1"),
      selectInput("scn_active", label = NULL, choices = NULL),
      uiOutput("scn_active_meta"),

      hr(class = "my-2"),

      tags$h6("Quick operation", class = "text-muted mb-1"),
      div(class = "form-text mb-2 mt-n1",
          "Applies to every row matching the target."),
      uiOutput("scn_target_inputs"),
      layout_columns(
        col_widths = c(5, 7),
        selectInput("op_kind", "Do", choices = c(
          "scale by"   = "scale",
          "add"        = "delta",
          "set each to" = "set",
          "make total" = "total")),
        numericInput("op_value", "Value", value = 1.1, step = 0.1)
      ),
      div(class = "form-text mb-2 mt-n2", textOutput("op_hint", inline = TRUE)),
      actionButton("op_stage", "Stage this change", icon = icon("wand-magic-sparkles"),
                   class = "btn-sm btn-outline-primary w-100"),

      hr(class = "my-2"),

      tags$h6("Save as scenario", class = "text-muted mb-1"),
      uiOutput("scn_pending_note"),
      textInput("new_name", "Name", placeholder = "required"),
      textInput("new_nickname", "Nickname", placeholder = "e.g. TV -20%"),
      selectInput("new_status", "Status",
                  choices = c("in development", "to review", "approved")),
      layout_columns(
        col_widths = c(7, 5),
        actionButton("scn_save", "Save scenario", icon = icon("code-branch"),
                     class = "btn-primary w-100"),
        actionButton("scn_discard", "Discard", icon = icon("rotate-left"),
                     class = "btn-outline-secondary w-100")
      )
    ),

    # ------------------------------------------------------------------
    # Main: the grid, then the chat
    # ------------------------------------------------------------------
    card(
      full_screen = TRUE,
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span(icon("table-cells"), " Plan grid"),
            uiOutput("grid_badge", inline = TRUE))
      ),
      card_body(
        class = "pt-2",
        div(class = "form-text mb-2",
            "Rows are line items, columns are weeks. Type in a cell and press ",
            tags$kbd("Enter"), " or click away. Edits stage until you save."),
        reactableOutput("scn_grid")
      )
    ),

    card(
      full_screen = TRUE,
      height = "460px",
      card_header(icon("robot"), " Ask for a change",
                  class = "bg-primary text-white"),
      chat_ui("plan_chat", height = "100%", fill = TRUE, enable_cancel = TRUE)
    )
  )
)
