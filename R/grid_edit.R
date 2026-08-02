# ---------------------------------------------------------------------------
# The plan grid: long <-> wide, and an editable reactable.
#
# mediaplanr stores a plan long (one row per line item x week). Planners read a
# grid: line items down the side, weeks across the top. Everything here exists
# to bridge those two shapes -- pivot for display, map edits back for storage.
#
# Cell identity is the plan's own line_item() key plus the week, so an edit
# survives sorting, filtering and re-rendering. It never depends on a row index,
# which would desync the moment the user sorts a column.
# ---------------------------------------------------------------------------

CELL_SEP <- "@@"

# Frozen columns need an opaque fill; this matches the brand page background.
STICKY_BG <- "#FBF6EC"

cell_key <- function(line_item, week) paste0(line_item, CELL_SEP, as.character(week))

split_cell_key <- function(key) {
  parts <- strsplit(key, CELL_SEP, fixed = TRUE)[[1]]
  list(line_item = parts[1], week = if (length(parts) > 1) parts[2] else NA_character_)
}

# Wide view of a plan: one row per line item, one column per week.
# Returns list(df, li_grain, weeks) -- df's first columns are the line item
# grain columns, the rest are weeks named by ISO date.
plan_wide <- function(plan) {
  d  <- plan@data
  lg <- mediaplanr::line_item_grain(plan)
  wk <- plan@week_col

  if (!length(wk)) {
    # No time dimension: the "grid" is just the plan, one spend column.
    out <- d[, c(lg, "planned_spend"), drop = FALSE]
    out$.li <- mediaplanr::line_item(d, lg)
    return(list(df = out, li_grain = lg, weeks = character(0), value_cols = "planned_spend"))
  }

  d$.li <- mediaplanr::line_item(d, lg)
  weeks <- sort(unique(d[[wk]]))
  wcols <- as.character(weeks)

  li_rows <- d[!duplicated(d$.li), c(lg, ".li"), drop = FALSE]
  li_rows <- li_rows[order(li_rows$.li), , drop = FALSE]

  mat <- matrix(0, nrow = nrow(li_rows), ncol = length(weeks),
                dimnames = list(li_rows$.li, wcols))
  idx_li <- match(d$.li, li_rows$.li)
  idx_wk <- match(as.character(d[[wk]]), wcols)
  mat[cbind(idx_li, idx_wk)] <- d$planned_spend

  out <- cbind(li_rows[, c(lg, ".li"), drop = FALSE],
               as.data.frame(mat, check.names = FALSE))
  rownames(out) <- NULL
  list(df = out, li_grain = lg, weeks = wcols, value_cols = wcols)
}

# Turn accumulated cell edits into the named numeric vector build_scenario()
# takes, keyed by the plan's FULL grain (line item + week).
#
# edits: named list/vector of cell_key -> value.
edits_to_vector <- function(plan, edits) {
  if (!length(edits)) return(numeric(0))
  d  <- plan@data
  lg <- mediaplanr::line_item_grain(plan)
  wk <- plan@week_col
  full_keys <- mediaplanr::line_item(d, plan@grain)
  li_keys   <- mediaplanr::line_item(d, lg)

  out <- numeric(0)
  for (k in names(edits)) {
    parts <- split_cell_key(k)
    if (!length(wk) || is.na(parts$week)) {
      hit <- which(li_keys == parts$line_item)
    } else {
      hit <- which(li_keys == parts$line_item &
                     as.character(d[[wk]]) == parts$week)
    }
    if (!length(hit)) next   # cell no longer exists (plan changed underneath)
    out[full_keys[hit[1]]] <- as.numeric(edits[[k]])
  }
  out
}

# The editable grid. Spend cells render as number inputs carrying their cell
# key; www/grid.js delegates their change events back to Shiny.
plan_grid <- function(plan, pending = list(), editable = TRUE) {
  w <- plan_wide(plan)
  df <- w$df
  lg <- w$li_grain

  # Overlay staged edits so the grid shows what the user typed, not what is
  # committed -- pending edits are not in the plan yet by design.
  if (length(pending)) {
    for (k in names(pending)) {
      p <- split_cell_key(k)
      r <- which(df$.li == p$line_item)
      cc <- if (is.na(p$week)) "planned_spend" else p$week
      if (length(r) && cc %in% names(df)) df[r[1], cc] <- as.numeric(pending[[k]])
    }
  }

  df$Total <- rowSums(df[, w$value_cols, drop = FALSE], na.rm = TRUE)

  # names(list()) is NULL, and toJSON(NULL) emits `{}` -- which would give the
  # cell renderer `{}.indexOf(...)` and break every cell. Force a JSON array.
  edited_keys <- as.character(names(pending))
  if (!length(edited_keys)) edited_keys <- character(0)

  value_col_def <- function(colname) {
    reactable::colDef(
      name  = if (colname == "planned_spend") "Spend" else format_week_header(colname),
      align = "right",
      minWidth = 92,
      cell = if (!editable) function(value) fmt_money_short(value) else
        reactable::JS(sprintf("
          function(cellInfo) {
            var key = cellInfo.row['.li'] + '%s' + '%s';
            var dirty = %s.indexOf(key) !== -1;
            return '<input type=\"number\" class=\"mp-cell' + (dirty ? ' mp-cell-dirty' : '') +
                   '\" data-key=\"' + key + '\" value=\"' + (cellInfo.value === null ? 0 : cellInfo.value) +
                   '\" step=\"1000\" min=\"0\">';
          }", CELL_SEP, colname, jsonlite::toJSON(edited_keys))),
      html = editable
    )
  }

  cols <- list()
  for (g in lg) {
    cols[[g]] <- reactable::colDef(
      name = tools::toTitleCase(g), minWidth = 110, sticky = "left",
      # Opaque: a translucent sticky column lets the scrolling body show
      # through it, which renders as overlapping text.
      style = list(fontWeight = 500, background = STICKY_BG,
                   borderRight = "1px solid rgba(0,0,0,0.08)"),
      headerStyle = list(background = STICKY_BG)
    )
  }
  cols[[".li"]] <- reactable::colDef(show = FALSE)
  for (v in w$value_cols) cols[[v]] <- value_col_def(v)
  cols[["Total"]] <- reactable::colDef(
    name = "Total", align = "right", minWidth = 118, sticky = "right",
    style = list(fontWeight = 600, background = STICKY_BG,
                 borderLeft = "1px solid rgba(0,0,0,0.12)"),
    headerStyle = list(background = STICKY_BG,
                       borderLeft = "1px solid rgba(0,0,0,0.12)"),
    cell = function(value) fmt_money(value)
  )

  reactable::reactable(
    df,
    columns     = cols,
    defaultPageSize = 25,
    highlight   = TRUE,
    compact     = TRUE,
    bordered    = TRUE,
    striped     = FALSE,
    wrap        = FALSE,
    resizable   = TRUE,
    sortable    = FALSE,      # sorting an editable grid invites mis-clicks
    style       = list(fontSize = "0.82rem")
  )
}

# "2026-04-06" -> "Apr 06" for a compact header.
format_week_header <- function(x) {
  d <- suppressWarnings(as.Date(x))
  if (is.na(d)) return(x)
  format(d, "%b %d")
}
