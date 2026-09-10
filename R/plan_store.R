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

  # The Build page's scaffold recipe. It lives here, rather than only in a
  # reactive, for the same reason the set does: the agent writes to it from
  # outside the reactive graph. `scaffold_n` counts the writes the AGENT made,
  # so the form can tell "the assistant changed this, push it into my inputs"
  # from its own echo and not loop.
  st$scaffold   <- NULL
  st$scaffold_n <- 0L

  # Set while a recipe is being authored FOR a cell of an existing plan
  # (list(parent=, key=)). It is what makes the Build button attach rather than
  # start a new session, and it is cleared the moment it is used.
  st$subplan_target <- NULL
  st
}

# The form's own writes; deliberately silent, so `get_scaffold` always reads
# what is on screen without the form and the agent chasing each other.
store_sync_scaffold <- function(st, recipe) {
  st$scaffold <- recipe
  invisible(recipe)
}

# The agent's writes, which the form must pick up.
store_set_scaffold <- function(st, recipe) {
  st$scaffold   <- recipe
  st$scaffold_n <- (st$scaffold_n %||% 0L) + 1L
  invisible(recipe)
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

# Promote a scenario through the review workflow. Status is a LABEL, not an
# edit: changing it must not fork the lineage, so the plan is rebuilt with the
# same id and parent_id and put back in place.
#
# One implementation, two callers -- the Review page's control and the agent's
# set_scenario_status tool -- so the UI and the assistant can never disagree
# about what approving means.
store_set_status <- function(st, name, status) {
  # revise(), not a hand-rebuilt MediaPlan(). Listing the slots by hand means
  # forgetting the ones added later: it dropped @subplans and reset @revision to
  # 1, so approving a topline deleted the detail plans hanging off it and turned
  # "Rev 3" back into "Rev 1", silently. revise() copies the whole plan and
  # changes only what it is asked to -- which is the door the package grew for
  # exactly this trap.
  store_replace(st, name, mediaplanr::revise(store_get(st, name), status = status))
}

# ---------------------------------------------------------------------------
# Structure
#
# A subplan lives INSIDE a MediaPlan, so the store needs no new container --
# attaching changes one plan in place. It is not a scenario and not an edit:
# nothing forks, and the parent keeps its id, which is why this goes through
# store_replace() rather than store_add().
# ---------------------------------------------------------------------------

store_attach_subplan <- function(st, parent_name, subplan) {
  parent <- store_get(st, parent_name)
  store_replace(st, parent_name, mediaplanr::attach_subplan(parent, subplan))
}

store_detach_subplan <- function(st, parent_name, key) {
  parent <- store_get(st, parent_name)
  store_replace(st, parent_name, mediaplanr::detach_subplan(parent, key))
}

# Which cells of a plan a subplan owns. Returns character(0) for a flat plan,
# which is the common case and not an error.
store_subplan_keys <- function(st, name = NULL) {
  if (!store_has_plan(st)) return(character(0))
  names(store_get(st, name)@subplans) %||% character(0)
}
