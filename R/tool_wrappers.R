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

# Same, but folds any WARNINGS into the result. .try() catches errors only, so a
# function that warns and returns -- compare_scenarios(level = "flight") on a
# set sharing no flight ids is the live example -- hands the model a clean
# ok = TRUE payload while the explanation goes to the R console, where no one
# reads it. Anything that can warn should use this.
.try_warn <- function(expr) {
  warnings <- character(0)
  out <- withCallingHandlers(
    tryCatch(expr, error = function(e) .err(conditionMessage(e))),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  if (length(warnings) && isTRUE(out$ok)) out$warnings <- warnings
  out
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

# Built on the package's own accessors rather than hand-rolled, so the model and
# R cannot drift apart. The old version pasted line item keys ("TV | NBC") and
# left the model to split strings to recover channel and partner -- exactly the
# guessing this tool exists to prevent. grain_values() returns each grain
# column's distinct values in the column's own type, which is precisely what an
# edit `target` accepts.
describe_plan_tool <- function(st, scenario = NULL) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded. Ask the user to load one on the Plan page."))
  p  <- store_get(st, scenario)
  d  <- p@data
  lg <- setdiff(mediaplanr::line_item_grain(p), mediaplanr::flight_cols())

  win <- mediaplanr::flight_window(p)
  fl  <- mediaplanr::flights(p)
  li  <- mediaplanr::line_item_summary(p)

  .ok(
    scenario    = scenario %||% st$active,
    name        = p@name,
    nickname    = p@nickname,
    status      = p@status,
    advertiser  = p@advertiser,
    planner     = p@planner,
    grain       = p@grain,
    line_item_grain = lg,

    # When the plan runs. A `during`, `shift` or `restage` is unanswerable
    # without this, and the old tool reported no dates at all.
    in_market   = if (length(win)) list(from = format(win[["start"]]),
                                        to   = format(win[["end"]]),
                                        days = p@flight_days) else NULL,
    week_col    = if (length(p@week_col)) p@week_col else NULL,
    week_start  = mediaplanr::week_start(p),

    # Every grain column's real values, ready to use as a target.
    values      = lapply(mediaplanr::grain_values(p), as.character),

    line_items  = .df_rows(li),
    flights     = if (nrow(fl)) .df_rows(fl) else NULL,
    units       = .units_summary(p),

    n_rows      = nrow(d),
    total_spend = sum(d$planned_spend)
  )
})

# A data frame as a list of row-lists, which is what serialises into a tool
# result the model can read without column/row confusion.
.df_rows <- function(df, max_rows = 60) {
  if (is.null(df) || !nrow(df)) return(NULL)
  df <- utils::head(df, max_rows)
  for (nm in names(df)) {
    if (inherits(df[[nm]], "Date")) df[[nm]] <- format(df[[nm]])
  }
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

# What the plan buys, per unit type, with the rate in its trade convention --
# a CPM for impressions, a per-unit cost otherwise.
.units_summary <- function(p) {
  d <- p@data
  if (!all(c("unit_type", "planned_units") %in% names(d))) return(NULL)
  ok <- !is.na(d$unit_type) & !is.na(d$planned_units)
  if (!any(ok)) return(NULL)
  ut <- as.character(d$unit_type)[ok]
  units <- tapply(d$planned_units[ok], ut, sum)
  spend <- tapply(d$planned_spend[ok], ut, sum)
  lapply(stats::setNames(names(units), names(units)), function(u) {
    per <- if (tolower(u) %in% c("impression", "impressions")) 1000 else 1
    list(planned_units = unname(units[[u]]),
         planned_spend = unname(spend[[u]]),
         rate = unname(spend[[u]] / units[[u]] * per),
         rate_basis = if (per == 1000) "CPM (per thousand)" else "per unit")
  })
}

list_scenarios_tool <- function(st) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))
  s <- mediaplanr::compare_scenarios(st$set, "summary")
  s$status <- vapply(st$set@scenarios[s$scenario], function(p) p@status, character(1))
  s$nickname <- vapply(st$set@scenarios[s$scenario], function(p) p@nickname, character(1))
  .ok(active = st$active, baseline = st$set@base_name,
      scenarios = lapply(seq_len(nrow(s)), function(i) as.list(s[i, ])))
})

compare_plans_tool <- function(st, level = "summary") .try_warn({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))
  if (length(st$set@scenarios) < 2) {
    return(.err("Only the baseline exists so far. Create a scenario first with apply_edits."))
  }
  tbl <- mediaplanr::compare_scenarios(st$set, level)
  # .df_rows() formats Date columns as strings; the flight level has three of
  # them, and a bare Date serialises as a number the model cannot read.
  .ok(level = level, n_rows = nrow(tbl), table = .df_rows(tbl, max_rows = 200))
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

# The buys a plan was authored from. Reports nothing rather than guessing when
# a plan records no flight identity -- four equal weeks are indistinguishable
# from one long buy, and the package refuses to invent one.
list_flights_tool <- function(st, scenario = NULL) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))
  p  <- store_get(st, scenario)
  fl <- mediaplanr::flights(p)
  if (!nrow(fl)) {
    return(.ok(scenario = scenario %||% st$active, flights = NULL,
               message = paste0(
                 "This plan records no flights: it was authored week by week, ",
                 "not as buys with in-market dates. Weekly rows cannot be ",
                 "turned back into flights -- four equal weeks could be one ",
                 "buy or four -- so there is nothing to report.")))
  }
  .ok(scenario = scenario %||% st$active, n = nrow(fl), flights = .df_rows(fl))
})

# Re-cut the plan onto a different calendar. Read-only: it answers "what does
# this look like by month" without creating a scenario.
calendar_view_tool <- function(st, basis = "month", scenario = NULL) .try({
  if (!store_has_plan(st)) return(.err("No plan loaded yet."))
  if (!basis %in% c("day", "week", "month")) {
    return(.err("basis must be one of: day, week, month"))
  }
  p <- store_get(st, scenario)
  if (!length(p@week_col)) {
    return(.err("This plan has no time dimension, so there is no calendar to cut it onto."))
  }
  tbl <- mediaplanr::calendarize(p, basis)
  .ok(scenario = scenario %||% st$active, basis = basis, n_rows = nrow(tbl),
      total_spend = sum(tbl$planned_spend), rows = .df_rows(tbl, max_rows = 120))
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
