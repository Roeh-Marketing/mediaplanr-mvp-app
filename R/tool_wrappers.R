# ---------------------------------------------------------------------------
# Plain functions the agent's tools call.
#
# Same contract as mrmopt-mvp-app: return .ok(...) or .err(...), never throw,
# so a tool failure comes back to the model as data it can reason about rather
# than an exception that kills the turn.
#
# These operate on the shared plan store, so anything the agent does shows up in
# the UI immediately -- and vice versa. There is one source of truth.
# ---------------------------------------------------------------------------

.err <- function(...) list(ok = FALSE, error = paste0(...))
.ok  <- function(...) c(list(ok = TRUE), list(...))

.try <- function(expr) {
  tryCatch(expr, error = function(e) .err(conditionMessage(e)))
}

# Render a ggplot as a chat-renderable tool result. Every element must be an
# ellmer Content object for the image to reach both the model and the chat UI
# (see the note in mrmopt-mvp-app/R/tool_wrappers.R).
.plot_content <- function(p, ..., width = 1000, height = 560, dpi = 96) {
  if (is.null(p)) return(.err("Nothing to plot for that selection."))
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = width, height = height, res = dpi)
  ok <- tryCatch({ print(p); TRUE }, error = function(e) e)
  grDevices::dev.off()
  if (!isTRUE(ok)) return(.err("Failed to render plot: ", conditionMessage(ok)))
  meta <- c(list(ok = TRUE), list(...))
  list(
    ellmer::ContentText(jsonlite::toJSON(meta, auto_unbox = TRUE)),
    ellmer::content_image_file(path, resize = "none")
  )
}

# --- read -------------------------------------------------------------------

describe_plan_tool <- function(st, scenario = NULL) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded. Ask the user to load one on the Plan page."))
  p  <- store_get(st, scenario)
  lg <- mediaplanr::line_item_grain(p)
  d  <- p@data
  .ok(
    scenario    = scenario %||% st$active,
    name        = p@name,
    nickname    = p@nickname,
    status      = p@status,
    advertiser  = p@advertiser,
    planner     = p@planner,
    grain       = p@grain,
    line_item_grain = lg,
    week_col    = if (length(p@week_col)) p@week_col else NULL,
    weeks       = if (length(p@week_col)) as.character(sort(unique(d[[p@week_col]]))) else NULL,
    line_items  = unique(mediaplanr::line_item(d, lg)),
    n_rows      = nrow(d),
    total_spend = sum(d$planned_spend),
    spend_by_line_item = {
      k <- mediaplanr::line_item(d, lg)
      as.list(tapply(d$planned_spend, k, sum))
    }
  )
})

list_scenarios_tool <- function(st) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))
  s <- mediaplanr::compare_scenarios(st$set, "summary")
  s$status <- vapply(st$set@scenarios[s$scenario], function(p) p@status, character(1))
  s$nickname <- vapply(st$set@scenarios[s$scenario], function(p) p@nickname, character(1))
  .ok(active = st$active, baseline = st$set@base_name,
      scenarios = lapply(seq_len(nrow(s)), function(i) as.list(s[i, ])))
})

compare_plans_tool <- function(st, level = "summary") .try({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))
  if (length(st$set@scenarios) < 2) {
    return(.err("Only the baseline exists so far. Create a scenario first with apply_edits."))
  }
  tbl <- mediaplanr::compare_scenarios(st$set, level)
  .ok(level = level, n_rows = nrow(tbl),
      table = lapply(seq_len(nrow(tbl)), function(i) as.list(tbl[i, ])))
})

# --- write ------------------------------------------------------------------

# The important one: the agent states an operation, R does the arithmetic.
# `edits` arrives as a JSON string because ellmer's typed arguments cannot
# express the recursive shape of an ops list.
apply_edits_tool <- function(st, edits_json, name, nickname = "",
                             status = "in development", objective = "",
                             from_scenario = NULL) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))

  ops <- tryCatch(jsonlite::fromJSON(edits_json, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(ops)) return(.err("`edits_json` is not valid JSON."))
  # A single op may arrive unwrapped; build_scenario accepts either, but be
  # explicit so an error message can say which op failed.
  if (!is.null(names(ops))) ops <- list(ops)

  # Resolve the parent label BEFORE adding: store_add() moves st$active to the
  # new scenario, so reading it afterwards would report the child as its own
  # parent.
  parent_label <- from_scenario %||% st$active
  parent <- store_get(st, parent_label)
  new <- mediaplanr::build_scenario(
    parent, edits = ops, name = name, nickname = nickname,
    status = status, objective = objective
  )
  label <- store_add(st, new)

  s <- mediaplanr::compare_scenarios(st$set, "summary")
  row <- s[s$scenario == label, ]
  .ok(scenario = label, derived_from = parent_label,
      total_spend = row$total_planned_spend,
      spend_vs_base = row$spend_vs_base,
      pct_vs_base = row$spend_pct_vs_base,
      message = paste0("Created scenario '", label, "'."))
})

set_status_tool <- function(st, scenario, status) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))
  if (!status %in% mediaplanr::status_levels()) {
    return(.err("status must be one of: ",
                paste(mediaplanr::status_levels(), collapse = ", ")))
  }
  p <- store_get(st, scenario)
  updated <- mediaplanr::MediaPlan(
    data = p@data, grain = p@grain, week_col = p@week_col,
    id = p@id, parent_id = p@parent_id, name = p@name,
    nickname = p@nickname, advertiser = p@advertiser, planner = p@planner,
    status = status, objective = p@objective
  )
  store_replace(st, scenario, updated)
  .ok(scenario = scenario, status = status)
})

roll_up_tool <- function(st, grain, scenario = NULL) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))
  p <- store_get(st, scenario)
  r <- mediaplanr::roll_up(p, grain)
  d <- r@data
  k <- mediaplanr::line_item(d, r@grain)
  .ok(grain = grain, n_rows = nrow(d),
      spend = as.list(tapply(d$planned_spend, k, sum)))
})

set_active_tool <- function(st, scenario) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))
  store_set_active(st, scenario)
  .ok(active = scenario)
})

# --- plot -------------------------------------------------------------------

plot_comparison_tool <- function(st, which = "totals", scenario = NULL) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))
  if (!which %in% c("totals", "mix", "flighting", "deltas")) {
    return(.err("which must be one of: totals, mix, flighting, deltas"))
  }
  p <- plan_chart(st$set, which, scenario = scenario)
  if (is.null(p)) {
    return(.err("Nothing to draw: ",
                if (which == "deltas") "pick a non-baseline scenario."
                else "the plan has no week column."))
  }
  .plot_content(p, chart = which, scenarios = names(st$set@scenarios))
})
