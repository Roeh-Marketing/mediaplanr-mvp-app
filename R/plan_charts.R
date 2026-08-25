# ---------------------------------------------------------------------------
# Comparison charts.
#
# Four views, each answering a different question a planner actually asks:
#   totals    -- how much are we spending, and how does that differ?
#   mix       -- where is the money going?
#   flighting -- when does it land?
#   deltas    -- which line items moved, and by how much?
#
# Static ggplot (plotly is not installed) with the Ro-eh palette from
# _brand.yml, so charts sit in the same colour language as the UI chrome.
# ---------------------------------------------------------------------------

BRAND <- list(
  midnight = "#15233F", slate = "#3A4A6B", stone = "#E8D8B8",
  sand     = "#C9A86A", gold  = "#E5B94E", cream = "#FBF6EC",
  ink      = "#2C2C2A", good  = "#3B6D11", bad   = "#A32D2D"
)

brand_pal <- function(n) {
  base <- c(BRAND$midnight, BRAND$sand, BRAND$slate, BRAND$gold,
            "#6B7FA3", "#8C6D3F", "#2E4460", "#C0AE86")
  if (n <= length(base)) base[seq_len(n)] else
    grDevices::colorRampPalette(base)(n)
}

theme_plan <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = BRAND$ink),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 1,
                                               colour = BRAND$midnight),
      plot.subtitle    = ggplot2::element_text(colour = BRAND$slate, size = base_size - 1),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_blank(),
      plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
}

# Long frame of the selected scenarios: scenario + grain columns + spend.
.set_long <- function(set, scenarios = NULL) {
  cmp <- mediaplanr::compare_scenarios(set, "cell")
  if (!is.null(scenarios)) cmp <- cmp[cmp$scenario %in% scenarios, , drop = FALSE]
  cmp$scenario <- factor(cmp$scenario, levels = unique(cmp$scenario))
  cmp
}

# --- 1. totals ---------------------------------------------------------------
chart_totals <- function(set, scenarios = NULL) {
  s <- mediaplanr::compare_scenarios(set, "summary")
  if (!is.null(scenarios)) s <- s[s$scenario %in% scenarios, , drop = FALSE]
  s$scenario <- factor(s$scenario, levels = s$scenario)
  s$is_base  <- s$scenario == set@base_name
  s$lbl <- ifelse(s$is_base, fmt_money_short(s$total_planned_spend),
                  paste0(fmt_money_short(s$total_planned_spend), "  (",
                         fmt_pct(s$spend_pct_vs_base), ")"))

  ggplot2::ggplot(s, ggplot2::aes(x = scenario, y = total_planned_spend,
                                  fill = is_base)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = lbl), vjust = -0.5,
                       size = 3.4, colour = BRAND$ink) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = BRAND$midnight,
                                          `FALSE` = BRAND$sand), guide = "none") +
    ggplot2::scale_y_continuous(labels = fmt_money_short,
                                expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(title = "Total planned spend",
                  subtitle = "Baseline in navy; deltas shown against it",
                  x = NULL, y = NULL) +
    theme_plan()
}

# --- 2. mix ------------------------------------------------------------------
chart_mix <- function(set, scenarios = NULL, by = NULL) {
  cmp <- .set_long(set, scenarios)
  by <- by %||% .dim_cols(set)[1]
  if (is.na(by) || !by %in% names(cmp)) by <- set@grain[1]

  agg <- stats::aggregate(cmp$planned_spend,
                          by = list(scenario = cmp$scenario, grp = cmp[[by]]),
                          FUN = sum)
  names(agg)[3] <- "planned_spend"

  ggplot2::ggplot(agg, ggplot2::aes(x = scenario, y = planned_spend, fill = grp)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::scale_fill_manual(values = brand_pal(length(unique(agg$grp)))) +
    ggplot2::scale_y_continuous(labels = fmt_money_short,
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(title = paste0("Mix by ", tools::toTitleCase(by)),
                  subtitle = "Where the money goes in each scenario",
                  x = NULL, y = NULL) +
    theme_plan()
}

# --- 3. flighting ------------------------------------------------------------
# Spend over time, cut onto a real calendar rather than onto the plan's storage
# grain. calendarize() spreads each row across the days it is actually in market
# before gathering, so a buy that starts mid-week contributes a part week
# instead of showing as a dip nobody planned, and a month view splits a week
# that straddles the boundary.
chart_flighting <- function(set, scenarios = NULL, basis = "week") {
  if (is.null(.week_col_of(set))) return(NULL)
  nms <- names(set@scenarios)
  if (!is.null(scenarios)) nms <- intersect(nms, scenarios)
  if (!length(nms)) return(NULL)

  parts <- lapply(nms, function(nm) {
    tbl <- try(mediaplanr::calendarize(set@scenarios[[nm]], basis), silent = TRUE)
    if (inherits(tbl, "try-error")) return(NULL)
    data.frame(scenario = nm, period = tbl[[basis]],
               planned_spend = tbl$planned_spend, stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(NULL)
  cmp <- do.call(rbind, parts)

  agg <- stats::aggregate(cmp$planned_spend,
                          by = list(scenario = cmp$scenario, period = cmp$period),
                          FUN = sum)
  names(agg)[3] <- "planned_spend"

  # Breaks are set per basis: a month view lands one tick on each month start
  # (the auto-breaks otherwise fall mid-month and repeat "Apr 2026"), and a day
  # view thins to fortnightly ticks so a quarter's worth of days does not
  # overprint its labels in the narrow Compare card.
  lab <- switch(basis, day = "%b %d", week = "%b %d", month = "%b %Y")
  brk <- switch(basis, day = "2 weeks", week = ggplot2::waiver(), month = "1 month")
  ggplot2::ggplot(agg, ggplot2::aes(x = period, y = planned_spend,
                                    colour = scenario, group = scenario)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = if (basis == "day") 0.9 else 1.6) +
    ggplot2::scale_colour_manual(values = brand_pal(length(unique(agg$scenario)))) +
    ggplot2::scale_y_continuous(labels = fmt_money_short,
                                expand = ggplot2::expansion(mult = c(0.05, 0.1))) +
    ggplot2::scale_x_date(date_labels = lab, date_breaks = brk) +
    ggplot2::labs(title = "Flighting",
                  subtitle = paste0("Total planned spend by ", basis),
                  x = NULL, y = NULL) +
    theme_plan()
}

# --- 4. deltas ---------------------------------------------------------------
# Per line item, one scenario against the baseline. This is the chart that
# answers "what actually changed", which the totals chart cannot show.
chart_deltas <- function(set, scenario) {
  if (identical(scenario, set@base_name)) return(NULL)
  cmp <- mediaplanr::compare_scenarios(set, "cell")
  cmp <- cmp[cmp$scenario == scenario, , drop = FALSE]
  if (!nrow(cmp)) return(NULL)

  lg <- .dim_cols(set)
  cmp$li <- do.call(paste, c(lapply(lg, function(g) as.character(cmp[[g]])),
                             list(sep = " | ")))
  agg <- stats::aggregate(cmp$spend_vs_base, by = list(li = cmp$li), FUN = sum)
  names(agg)[2] <- "delta"
  agg <- agg[agg$delta != 0, , drop = FALSE]
  if (!nrow(agg)) return(NULL)

  agg <- agg[order(agg$delta), ]
  agg$li <- factor(agg$li, levels = agg$li)
  agg$dir <- agg$delta > 0

  ggplot2::ggplot(agg, ggplot2::aes(x = delta, y = li, fill = dir)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_vline(xintercept = 0, colour = BRAND$ink, linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = fmt_money_short(delta),
                   hjust = ifelse(dir, -0.15, 1.15)),
      size = 3.1, colour = BRAND$ink) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = BRAND$good, `FALSE` = BRAND$bad),
                               guide = "none") +
    ggplot2::scale_x_continuous(labels = fmt_money_short,
                                expand = ggplot2::expansion(mult = 0.18)) +
    ggplot2::labs(title = paste0("What changed: ", scenario),
                  subtitle = "Line item spend versus the baseline",
                  x = NULL, y = NULL) +
    theme_plan()
}

# The week column of a set, taken from its baseline plan (every scenario in a
# set shares a grain, so any of them would do).
# Grain columns that describe a line item, excluding the week and the columns
# mediaplanr reserves. Without the second exclusion a plan carrying flights
# could end up with "Mix by Flight_id" or delta labels reading
# "TV | NBC | fl_2026... | even".
.dim_cols <- function(set) {
  setdiff(set@grain, c(.week_col_of(set), mediaplanr::flight_cols(),
                       mediaplanr::unit_cols()))
}

.week_col_of <- function(set) {
  p <- set@scenarios[[set@base_name]]
  if (length(p@week_col)) p@week_col else NULL
}

# Dispatch used by both the Compare page and the agent's plot tool.
plan_chart <- function(set, which = c("totals", "mix", "flighting", "deltas"),
                       scenarios = NULL, scenario = NULL, basis = "week") {
  which <- match.arg(which)
  switch(which,
    totals    = chart_totals(set, scenarios),
    mix       = chart_mix(set, scenarios),
    flighting = chart_flighting(set, scenarios, basis = basis),
    deltas    = chart_deltas(set, scenario %||% utils::tail(names(set@scenarios), 1))
  )
}
