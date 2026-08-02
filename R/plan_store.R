# ---------------------------------------------------------------------------
# Session store for plans.
#
# Mirrors mrmopt-mvp-app's registry.R: an environment holding session state,
# with session-scoped writers installed by server.R so the ellmer tools and the
# UI act on the same objects.
#
# The store holds a mediaplanr::ScenarioSet. The set IS the state -- there is no
# parallel bookkeeping of plans, because the set already knows its scenarios,
# its baseline, and (via each plan's @parent_id) its lineage.
# ---------------------------------------------------------------------------

new_plan_store <- function() {
  st <- new.env(parent = emptyenv())
  st$set    <- NULL   # mediaplanr::ScenarioSet
  st$active <- NULL   # name of the scenario currently being edited
  st
}

store_has_plan <- function(st) !is.null(st$set)

store_scenario_names <- function(st) {
  if (is.null(st$set)) character(0) else names(st$set@scenarios)
}

store_get <- function(st, name = NULL) {
  if (is.null(st$set)) stop("No plan loaded yet.", call. = FALSE)
  name <- name %||% st$active %||% st$set@base_name
  p <- st$set@scenarios[[name]]
  if (is.null(p)) {
    stop("No scenario named '", name, "'. Available: ",
         paste(store_scenario_names(st), collapse = ", "), call. = FALSE)
  }
  p
}

store_base <- function(st) {
  if (is.null(st$set)) stop("No plan loaded yet.", call. = FALSE)
  st$set@scenarios[[st$set@base_name]]
}

# Start a fresh set from a base plan, discarding anything previously loaded.
store_init <- function(st, plan) {
  st$set <- mediaplanr::scenario_set(plan)
  st$active <- st$set@base_name
  invisible(st)
}

store_add <- function(st, plan, name = NULL) {
  st$set <- mediaplanr::add_scenario(st$set, plan, name = name)
  # The set may have de-duplicated the label; the new one is always last.
  st$active <- utils::tail(names(st$set@scenarios), 1)
  st$active
}

store_set_active <- function(st, name) {
  if (!name %in% store_scenario_names(st)) {
    stop("No scenario named '", name, "'.", call. = FALSE)
  }
  st$active <- name
  invisible(name)
}

# Replace a scenario in place -- used only for metadata edits (status, planner)
# where deriving a new plan would be wrong: changing a label is not a new
# scenario, and doing so would fork the lineage for nothing.
store_replace <- function(st, name, plan) {
  scen <- st$set@scenarios
  if (!name %in% names(scen)) stop("No scenario named '", name, "'.", call. = FALSE)
  scen[[name]] <- plan
  st$set <- mediaplanr::ScenarioSet(
    scenarios = scen,
    grain     = st$set@grain,
    base_name = st$set@base_name,
    id        = st$set@id
  )
  invisible(plan)
}

store_reset <- function(st) {
  st$set <- NULL
  st$active <- NULL
  invisible(st)
}
