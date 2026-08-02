library(shiny)
library(bslib)
library(shinychat)
library(reactable)

app_ui <- page_navbar(
  id    = "main_nav",
  theme = bs_theme(version = 5, brand = TRUE),
  title = "mediaplanr — Plan Workbench",

  header = tags$head(
    tags$script(src = "grid.js"),
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
      .card-header    { font-weight: 600; }
      .kpi-value      { font-size: 1.45rem; font-weight: 600; line-height: 1.1; }
      .kpi-label      { font-size: 0.75rem; text-transform: uppercase;
                        letter-spacing: 0.04em; opacity: 0.65; }
    "))
  ),

  source("R/ui_panels/panel_welcome.R",   local = TRUE)$value,
  source("R/ui_panels/panel_plan.R",      local = TRUE)$value,
  source("R/ui_panels/panel_scenarios.R", local = TRUE)$value,
  source("R/ui_panels/panel_compare.R",   local = TRUE)$value,
  source("R/ui_panels/panel_export.R",    local = TRUE)$value,
  nav_spacer(),
  nav_item(uiOutput("nav_status"))
)
