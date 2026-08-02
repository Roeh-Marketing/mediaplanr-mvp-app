library(shiny)
library(bslib)
library(shinychat)
library(ellmer)
library(reactable)
library(ggplot2)
library(purrr)
library(promises)
library(mediaplanr)
# NB: jsonlite is deliberately NOT attached -- it masks shiny::validate (and
# purrr::flatten). Every use here is qualified as jsonlite::.

# ---------------------------------------------------------------------------
# mediaplanr — Plan Workbench
#
# Upload a media plan, edit it, fork scenarios, compare them.
#
# All plan semantics (validation, grain, line items, lineage, scenario
# derivation, comparison) come from the mediaplanr package. This app is I/O,
# layout and interaction only, and does no modelling — forecasting and budget
# optimisation live in the companion mrmopt app.
#
# The assistant needs ANTHROPIC_API_KEY. Without it the chat explains itself and
# every other feature still works.
# ---------------------------------------------------------------------------

source("R/app_helpers.R")
source("R/sample_data.R")
source("R/plan_store.R")
source("R/plan_io.R")
source("R/grid_edit.R")
source("R/plan_charts.R")
source("R/tool_wrappers.R")
source("R/ellmer_tools.R")
source("R/ui.R")
source("R/server_panels/server_welcome.R")
source("R/server_panels/server_plan.R")
source("R/server_panels/server_scenarios.R")
source("R/server_panels/server_compare.R")
source("R/server_panels/server_export.R")
source("R/server.R")

# Keep the sample CSV on disk in step with the generator, so the upload path can
# be exercised with a real file.
if (!file.exists("data/sample_media_plan.csv")) {
  dir.create("data", showWarnings = FALSE)
  write_sample_csv()
}

shinyApp(app_ui, app_server)
