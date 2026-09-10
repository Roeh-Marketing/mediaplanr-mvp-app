# ---------------------------------------------------------------------------
# Small shared helpers: formatting, column guessing, empty states.
# ---------------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

fmt_money <- function(x, digits = 0) {
  paste0("$", formatC(x, format = "f", big.mark = ",", digits = digits))
}

fmt_money_short <- function(x) {
  ifelse(abs(x) >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
  ifelse(abs(x) >= 1e3, paste0("$", round(x / 1e3, 0), "K"),
         paste0("$", round(x))))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "-", paste0(ifelse(x > 0, "+", ""), round(x * 100, digits), "%"))
}

fmt_delta <- function(x) {
  ifelse(x == 0, "-", paste0(ifelse(x > 0, "+", "-"), fmt_money(abs(x))))
}

# Guess which uploaded column is which, so the mapping controls start correct
# for a conventionally-named file and the user only intervenes when it matters.
guess_cols <- function(df) {
  nms <- names(df)
  low <- tolower(nms)

  pick <- function(pats) {
    for (p in pats) {
      hit <- which(grepl(p, low))
      if (length(hit)) return(nms[hit[1]])
    }
    NULL
  }

  spend <- pick(c("^planned_spend$", "^spend$", "budget", "cost", "investment"))
  # NB: no "period" pattern here -- it matches the reserved `period_basis`
  # column, which is not a week and is not even dateish, so the seeded
  # selection would silently fail to apply.
  week  <- pick(c("^week$", "^date$", "week_start", "^wk$"))

  units <- pick(c("^planned_units$", "^units$", "impression", "click", "grp",
                  "^spots?$", "delivery"))
  rate  <- pick(c("^planned_rate$", "^rate$", "^cpm$", "^cpc$", "^cpp$",
                  "unit_cost"))
  unit_type <- pick(c("^unit_type$", "^unit$", "buying_unit", "^medium$"))

  # Anything character/factor with a sane number of distinct values is a
  # plausible grain column -- except the columns mediaplanr reserves. Those are
  # character too, so without this they get auto-selected INTO the grain, from
  # where they pollute the grid's sticky columns, the mix chart's dimension,
  # the delta chart's labels and the Compare cell table.
  reserved <- c(mediaplanr::flight_cols(), mediaplanr::unit_cols())
  cand <- nms[vapply(df, function(c) is.character(c) || is.factor(c), logical(1))]
  cand <- setdiff(cand, c(spend, week, units, rate, unit_type, reserved))
  grain <- cand[vapply(cand, function(c) {
    k <- length(unique(df[[c]]))
    k > 1 && k <= max(50, nrow(df) / 2)
  }, logical(1))]

  list(spend = spend, week = week, grain = grain,
       units = units, rate = rate, unit_type = unit_type)
}

# Is a column safely coercible to Date? Used to decide whether to offer it as
# the week column.
is_dateish <- function(x) {
  # POSIXct matters: readxl returns every date cell as POSIXct, so without this
  # an exported workbook re-uploads with its week column not even offered in
  # the mapping dropdown -- the round trip the README advertises silently
  # loses the plan's time dimension.
  if (inherits(x, "Date") || inherits(x, "POSIXct")) return(TRUE)
  if (!is.character(x) && !is.factor(x)) return(FALSE)
  v <- suppressWarnings(tryCatch(as.Date(as.character(x)),
                                 error = function(e) NA))
  !all(is.na(v))
}

# Units are counts, not money: thousands separators, no currency, and a short
# form once they get large -- 28M impressions reads better than 28,000,000.
fmt_units <- function(x) {
  if (!length(x) || is.na(x)) return("—")
  if (x >= 1e9) return(paste0(round(x / 1e9, 1), "B"))
  if (x >= 1e6) return(paste0(round(x / 1e6, 1), "M"))
  if (x >= 1e4) return(paste0(round(x / 1e3, 1), "K"))
  formatC(x, format = "f", big.mark = ",", digits = 0)
}

# A rate needs more precision than a budget: a CPC of $0.80 must not round to
# $1, and a CPM of $5.25 must not round to $5.
fmt_rate <- function(x) {
  if (!length(x) || is.na(x)) return("—")
  paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
}

# A centred muted message for cards with nothing to show yet.
empty_state <- function(msg, icon_name = "inbox") {
  div(
    class = "d-flex flex-column align-items-center justify-content-center text-muted py-5",
    icon(icon_name, class = "fa-2x mb-2 opacity-50"),
    div(class = "small", msg)
  )
}

# Status -> bootstrap badge class, so the same colour language is used wherever
# a status appears.
status_badge <- function(status) {
  if (is.null(status) || !nzchar(status)) {
    return(tags$span(class = "badge bg-light text-muted", "no status"))
  }
  cls <- switch(status,
    "approved"       = "bg-success",
    "to review"      = "bg-warning text-dark",
    "in development" = "bg-secondary",
    "bg-light text-muted")
  tags$span(class = paste("badge", cls), status)
}

# ---------------------------------------------------------------------------
# Chat plot plumbing (ported from mrmopt-mvp-app).
#
# shinychat's markdown renderer strips `data:` URIs from <img>, so images a tool
# returns are written to a per-session served directory and referenced by a real
# URL instead.
# ---------------------------------------------------------------------------

# Pull every inline image out of a chat's tool results from `from_turn` on.
extract_chat_images <- function(chat, from_turn = 1L) {
  turns <- tryCatch(chat$get_turns(), error = function(e) list())
  uris  <- character(0)
  if (length(turns) < from_turn) return(uris)
  for (ti in seq.int(from_turn, length(turns))) {
    contents <- tryCatch(turns[[ti]]@contents, error = function(e) NULL)
    for (co in contents) {
      if (inherits(co, "ellmer::ContentToolResult")) {
        val <- tryCatch(co@value, error = function(e) NULL)
        if (is.list(val)) for (v in val) {
          if (inherits(v, "ellmer::ContentImageInline")) {
            uris <- c(uris, paste0("data:", v@type, ";base64,", v@data))
          }
        }
      }
    }
  }
  uris
}

# Returns save_chat_plot(data_uri) -> served URL, scoped to this session.
make_chat_plot_saver <- function(session) {
  plot_dir <- file.path(tempdir(), paste0("planchat_", session$token))
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  prefix <- paste0("planchat-plots-", substr(session$token, 1, 8))
  shiny::addResourcePath(prefix, plot_dir)
  session$onSessionEnded(function() {
    try(shiny::removeResourcePath(prefix), silent = TRUE)
    try(unlink(plot_dir, recursive = TRUE), silent = TRUE)
  })
  function(data_uri) {
    b64 <- sub("^data:[^,]*,", "", data_uri)
    raw <- tryCatch(base64enc::base64decode(b64), error = function(e) NULL)
    if (is.null(raw)) return(NULL)
    fn <- paste0(format(Sys.time(), "%H%M%S"), "-", sample.int(1e6, 1), ".png")
    writeBin(raw, file.path(plot_dir, fn))
    paste0(prefix, "/", fn)
  }
}

# ---------------------------------------------------------------------------
# KPI strip.
#
# One definition, three callers (Build's registry, Review's preview, Compare).
# It used to be a local `kpi()` copied into each server module, which is how the
# Plan page and the Compare page ended up with the same card rendered from two
# places.
# ---------------------------------------------------------------------------

kpi_card <- function(label, value, sub = NULL) {
  card(class = "border-0 shadow-sm",
       card_body(class = "py-3",
                 div(class = "kpi-label", label),
                 div(class = "kpi-value", value),
                 if (!is.null(sub)) div(class = "small text-muted", sub)))
}

# The four (or five) numbers that describe one plan. "Weeks" was a count of the
# storage grain; the in-market window is what a planner actually asks, and it is
# the plan's own extent, so it stays true however the plan was authored.
plan_kpi_strip <- function(p) {
  d  <- p@data
  lg <- setdiff(mediaplanr::line_item_grain(p), mediaplanr::flight_cols())
  li <- length(unique(mediaplanr::line_item(d, lg)))
  fl <- nrow(mediaplanr::flights(p))
  win <- mediaplanr::flight_window(p)

  in_market <- if (length(win)) {
    kpi_card("In market",
             paste0(format(win[["start"]], "%d %b"), " - ", format(win[["end"]], "%d %b")),
             paste0(p@flight_days, " days",
                    if (!is.na(mediaplanr::week_start(p)))
                      paste0(" - ", mediaplanr::week_start(p), " weeks") else ""))
  } else {
    kpi_card("In market", "—", "no time dimension")
  }

  # Only shown when the plan records them, in the same "omit rather than show
  # empty" spirit the package's own print method uses.
  units_kpi <- NULL
  if (all(c("unit_type", "planned_units") %in% names(d))) {
    ok <- !is.na(d$unit_type) & !is.na(d$planned_units)
    if (any(ok)) {
      ut  <- as.character(d$unit_type)[ok]
      tot <- tapply(d$planned_units[ok], ut, sum)
      big <- names(tot)[which.max(tot)]
      sp  <- sum(d$planned_spend[ok & d$unit_type == big])
      per <- if (tolower(big) %in% c("impression", "impressions")) 1000 else 1
      units_kpi <- kpi_card(
        tools::toTitleCase(big),
        fmt_units(tot[[big]]),
        paste0(fmt_rate(sp / tot[[big]] * per), if (per == 1000) " CPM" else " each"))
    }
  }

  cells <- c(list(
    kpi_card("Total planned", fmt_money(sum(d$planned_spend))),
    kpi_card("Line items", li,
             if (fl) paste0(fl, " flight", if (fl == 1) "" else "s") else NULL),
    in_market,
    kpi_card("Rows", nrow(d))), if (!is.null(units_kpi)) list(units_kpi))
  n <- length(cells)
  do.call(layout_columns,
          c(list(col_widths = if (n == 5) c(3, 2, 3, 1, 3) else c(3, 3, 3, 3)), cells))
}

# ---------------------------------------------------------------------------
# Build-page tables.
# ---------------------------------------------------------------------------

# The file (or sample) exactly as read, before any mapping is applied. This is
# what tells the user what they just loaded, so it sits at the TOP of the build
# flow rather than under the outputs it explains.
raw_preview_table <- function(df, n = 10) {
  reactable::reactable(utils::head(df, n), compact = TRUE, bordered = TRUE,
                       pagination = FALSE, wrap = FALSE,
                       style = list(fontSize = "0.78rem"))
}

# The parsed outline, one row per line item.
#
# This is what carries the outline format: the textarea is terse, and this is
# where a planner sees that TV -> {NBC, ESPN} plus Search -> {Google} is three
# line items and not six, and what each one was actually given.
outline_table <- function(o) {
  if (!nrow(o)) {
    return(reactable::reactable(o, compact = TRUE, bordered = TRUE,
                                pagination = FALSE))
  }
  cols <- list(
    amount = reactable::colDef(name = "Amount", align = "right",
                               cell = function(v) fmt_money(v)),
    # Not fmt_pct(): that signs its output for deltas against a baseline, and
    # a share of the total is not a change in anything.
    share  = reactable::colDef(name = "Share", align = "right",
                               cell = function(v) {
                                 if (is.na(v)) "-" else sprintf("%.1f%%", v * 100)
                               }),
    pacing = reactable::colDef(name = "Pacing"),
    weeks  = reactable::colDef(name = "Weeks", align = "right")
  )
  reactable::reactable(o, compact = TRUE, bordered = TRUE, pagination = FALSE,
                       wrap = FALSE, columns = cols,
                       style = list(fontSize = "0.78rem"))
}

# What is in the session, one row per scenario, with its metadata rather than
# just its spend -- the plan's identity (advertiser, planner) is what someone
# scanning the registry is looking for.
#
# Status renders as a <select class="mp-status">; www/grid.js delegates its
# change event to `input$registry_status`, the same shim the editable grid uses.
registry_table <- function(set, base_name = NULL) {
  nms <- names(set@scenarios)
  s   <- mediaplanr::compare_scenarios(set, "summary")
  s   <- s[match(nms, s$scenario), , drop = FALSE]
  get <- function(f) vapply(set@scenarios[nms], f, character(1))

  tbl <- data.frame(
    Scenario   = nms,
    Name       = get(function(p) p@name),
    Nickname   = get(function(p) p@nickname),
    Advertiser = get(function(p) p@advertiser),
    Planner    = get(function(p) p@planner),
    Status     = get(function(p) p@status),
    Spend      = s$total_planned_spend,
    Rows       = vapply(set@scenarios[nms], function(p) nrow(p@data), integer(1)),
    Lineage    = ifelse(nms == (base_name %||% set@base_name), "baseline", "derived"),
    stringsAsFactors = FALSE, check.names = FALSE)
  # vapply() over a named list returns a NAMED vector, which data.frame() turns
  # into row names -- and reactable shows row names whenever it finds them, so
  # without this the table grows a nameless leading column of scenario labels.
  rownames(tbl) <- NULL

  levels_js <- jsonlite::toJSON(mediaplanr::status_levels())

  reactable::reactable(
    tbl, compact = TRUE, bordered = TRUE, highlight = TRUE, pagination = FALSE,
    columns = list(
      Scenario   = reactable::colDef(minWidth = 130, style = list(fontWeight = 600)),
      Name       = reactable::colDef(minWidth = 160),
      Nickname   = reactable::colDef(minWidth = 110,
                                     style = list(color = "#6c757d")),
      Advertiser = reactable::colDef(minWidth = 110),
      Planner    = reactable::colDef(minWidth = 100),
      Status     = reactable::colDef(minWidth = 150, html = TRUE,
        cell = reactable::JS(sprintf("
          function(cellInfo) {
            var levels = %s;
            var opts = levels.map(function (l) {
              return '<option value=\"' + l + '\"' +
                     (l === cellInfo.value ? ' selected' : '') + '>' + l + '</option>';
            }).join('');
            return '<select class=\"mp-status\" data-scenario=\"' +
                   cellInfo.row['Scenario'] + '\">' + opts + '</select>';
          }", levels_js))),
      Spend      = reactable::colDef(align = "right", minWidth = 110,
                                     cell = function(v) fmt_money(v)),
      Rows       = reactable::colDef(align = "right", minWidth = 70),
      Lineage    = reactable::colDef(minWidth = 90, cell = function(v) {
        htmltools::tags$span(class = "small text-muted", v)
      })
    ),
    style = list(fontSize = "0.82rem"))
}
