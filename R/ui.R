library(shiny)
library(bslib)
library(shinychat)
library(reactable)

app_ui <- page_navbar(
  id    = "main_nav",
  theme = bs_theme(version = 5, brand = TRUE),
  title = "mediaplanr — Plan Workbench",

  # Every page here is a document to scroll, not a dashboard to fit: the Build
  # page grows as its accordions open, and Review stacks a grid under two
  # charts. Left fillable, bslib gives each panel a viewport-height card whose
  # BODY scrolls -- so the page and the card scroll independently and the top of
  # the card goes blank as you scroll past it. One scrollbar is the whole fix.
  fillable = FALSE,

  header = tags$head(
    # Versioned by the file's own mtime. Without this the browser holds on to a
    # cached copy after the file changes, and the delegated listeners in it go
    # quietly missing -- the page looks fine and nothing reaches the server.
    tags$script(src = paste0(
      "grid.js?v", as.integer(file.mtime("www/grid.js")))),
    tags$style(HTML("
      /* Editable spend cells: look like a spreadsheet, not a form. */
      .mp-cell {
        width: 100%; border: 1px solid transparent; background: transparent;
        text-align: right; font-variant-numeric: tabular-nums;
        padding: 1px 4px; border-radius: 3px; font-size: 0.82rem;
        -moz-appearance: textfield;
      }
      .mp-cell::-webkit-outer-spin-button,
      .mp-cell::-webkit-inner-spin-button { -webkit-appearance: none; margin: 0; }
      .mp-cell:hover  { border-color: rgba(0,0,0,0.15); background: #fff; }
      .mp-cell:focus  { border-color: #15233F; background: #fff; outline: none;
                        box-shadow: 0 0 0 2px rgba(21,35,63,0.12); }
      .mp-cell-dirty  { background: #FFF6DC !important; border-color: #E5B94E !important;
                        font-weight: 600; }
      .mp-cell-bad    { background: #FBE3E3 !important; border-color: #A32D2D !important; }

      /* A cell a subplan owns. Its number is the subplan's rollup and every
         edit door refuses it, so it is shown as a value rather than offered as
         an input -- the grid should not invite an edit the package rejects. */
      .mp-cell-locked { display: block; text-align: right; padding: 1px 4px;
                        font-variant-numeric: tabular-nums; font-size: 0.82rem;
                        color: rgba(0,0,0,0.55); background: rgba(0,0,0,0.035);
                        border: 1px dashed rgba(0,0,0,0.18); border-radius: 3px; }
      .mp-owned       { opacity: 0.55; font-size: 0.72rem; }
      .card-header    { font-weight: 600; }

      /* The build/review action bars: source on the left, the primary action on
         the right, banded off from the content below.
         Not `position: sticky` -- bslib gives every card body `overflow: auto`,
         which makes it the sticky ancestor, and that container never scrolls
         (the page does), so a sticky bar in here would only ever look stuck
         while behaving normally. It sits at the top of its tab instead, which
         is where it is needed. */
      .mp-actionbar   { background: var(--bs-body-bg, #FBF6EC);
                        padding: 0.6rem 0; margin-bottom: 0.25rem;
                        border-bottom: 1px solid rgba(0,0,0,0.08); }
      .mp-actionbar .form-group { margin-bottom: 0; }
      .mp-upload      { min-width: 260px; max-width: 340px; }
      .mp-upload .form-group { margin-bottom: 0; }

      /* Inline status editor in the registry table. */
      .mp-status      { font-size: 0.75rem; padding: 1px 4px; border-radius: 10px;
                        border: 1px solid rgba(0,0,0,0.15); background: #fff; }
      .mp-status:focus { outline: none; border-color: #15233F;
                         box-shadow: 0 0 0 2px rgba(21,35,63,0.12); }
      .kpi-value      { font-size: 1.45rem; font-weight: 600; line-height: 1.1; }
      .kpi-label      { font-size: 0.75rem; text-transform: uppercase;
                        letter-spacing: 0.04em; opacity: 0.65; }

      /* The assistant drawer. One conversation, reachable from every page,
         and costing no width until it is asked for -- which is what the Build
         page's wide accordions and the Edit grid both wanted.
         The body is a flex column so the transcript takes the slack and the
         composer stays at the bottom; without the min-height:0 the chat grows
         past the drawer instead of scrolling inside it. */
      #mp_chat_drawer { --bs-offcanvas-width: 430px; }
      #mp_chat_drawer .offcanvas-body {
        display: flex; flex-direction: column; padding-top: 0.5rem;
        overflow: hidden;
      }
      #mp_chat_drawer shiny-chat-container {
        flex: 1 1 auto; min-height: 0;
      }
      .mp-drawer-hint { font-size: 0.78rem; opacity: 0.7; }

      /* Skill chips: what the assistant can do, visible without opening it. */
      .mp-skill {
        border: 1px solid rgba(0,0,0,0.15); background: #fff;
        border-radius: 999px; padding: 3px 12px; font-size: 0.78rem;
        color: inherit; line-height: 1.6;
      }
      .mp-skill:hover  { border-color: #15233F; background: #F4F0E6; }
      .mp-skill:focus  { outline: none; box-shadow: 0 0 0 2px rgba(21,35,63,0.15); }
      .mp-skill .fa, .mp-skill .fas, .mp-skill .far { opacity: 0.6; margin-right: 4px; }
      .mp-skill-label  { vertical-align: middle; }
    "))
  ),

  # Two halves: tabs where things change (Build, Edit) and tabs where they are
  # read (Review, Export).
  source("R/ui_panels/panel_welcome.R",   local = TRUE)$value,
  source("R/ui_panels/panel_build.R",     local = TRUE)$value,
  source("R/ui_panels/panel_scenarios.R", local = TRUE)$value,
  source("R/ui_panels/panel_review.R",    local = TRUE)$value,
  source("R/ui_panels/panel_export.R",    local = TRUE)$value,
  nav_spacer(),
  nav_item(uiOutput("nav_status")),

  # The trigger. Plain Bootstrap markup rather than an actionButton: opening a
  # drawer is a client-side concern and should not need a server round trip.
  nav_item(
    tags$button(
      class = "btn btn-sm btn-outline-light ms-2", type = "button",
      `data-bs-toggle` = "offcanvas", `data-bs-target` = "#mp_chat_drawer",
      icon("robot"), " Assistant")
  ),

  # Outside every nav_panel, which is exactly what makes it global: one chat,
  # one transcript, reachable from Build, Edit, Review and Export alike.
  footer = tags$div(
    class = "offcanvas offcanvas-end", tabindex = "-1", id = "mp_chat_drawer",
    `aria-labelledby` = "mp_chat_drawer_title",
    div(
      class = "offcanvas-header pb-1",
      tags$h6(class = "offcanvas-title mb-0", id = "mp_chat_drawer_title",
              icon("robot"), " Assistant"),
      tags$button(type = "button", class = "btn-close",
                  `data-bs-dismiss` = "offcanvas", `aria-label` = "Close")
    ),
    div(
      class = "offcanvas-body",
      div(class = "mp-drawer-hint mb-2", uiOutput("drawer_hint")),
      chat_ui("plan_chat", height = "100%", fill = TRUE, enable_cancel = TRUE)
    )
  )
)
