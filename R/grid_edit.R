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

# A grid edit becomes an OPERATION, not a raw cell value.
#
# The distinction that matters: a cell the plan actually has becomes a `set` on
# that one grain cell, while a cell the grid drew but the plan has no row for --
# a week a buy does not run -- becomes an `add`. The grid is a dense matrix over
# every line item x week, so on a flight-authored plan most cells are the second
# kind. Before this they were silently swallowed on save.
cell_edit_op <- function(plan, key, value) {
  d  <- plan@data
  lg <- setdiff(mediaplanr::line_item_grain(plan), mediaplanr::flight_cols())
  wk <- plan@week_col
  parts <- split_cell_key(key)

  li_keys <- mediaplanr::line_item(d, lg)
  hit <- if (!length(wk) || is.na(parts$week)) {
    which(li_keys == parts$line_item)
  } else {
    which(li_keys == parts$line_item & as.character(d[[wk]]) == parts$week)
  }

  if (length(hit)) {
    # The row exists: target it by its full grain so the op is unambiguous.
    target <- lapply(stats::setNames(plan@grain, plan@grain),
                     function(g) as.character(d[[g]][hit[1]]))
    return(list(target = target, set = as.numeric(value)))
  }

  # No such row. Recover the line item's values from any other row of it, so
  # the added row lands on the same line item rather than a new one.
  src <- which(li_keys == parts$line_item)
  if (!length(src)) return(NULL)          # unknown line item; nothing to add to
  spec <- lapply(stats::setNames(lg, lg), function(g) as.character(d[[g]][src[1]]))
  if (length(wk) && !is.na(parts$week)) spec[[wk]] <- parts$week
  spec$planned_spend <- as.numeric(value)
  list(add = spec)
}

# Ops are applied in order, so typing repeatedly in one cell would append a new
# `set` every time. Replace instead: same cell, same op kind, one entry.
stage_op <- function(ops, op) {
  k <- .op_cell_key(op)
  if (!is.null(k)) {
    keep <- vapply(ops, function(o) !identical(.op_cell_key(o), k), logical(1))
    ops <- ops[keep]
  }
  c(ops, list(op))
}

# The single grain cell an op addresses, or NULL if it addresses more than one.
# Only these coalesce; a broad operation is a distinct act each time.
.op_cell_key <- function(op) {
  if (!is.null(op$add)) {
    return(paste0("add:", paste(unlist(op$add[setdiff(names(op$add), "planned_spend")]),
                                collapse = "\u001f")))
  }
  if (is.null(op$set) || is.null(op$target)) return(NULL)
  paste0("set:", paste(unlist(op$target), collapse = "\u001f"))
}

# One staged operation, in words, for the review list.
describe_op <- function(op, plan) {
  money <- function(x) fmt_money(as.numeric(x))
  where <- function(t) {
    if (is.null(t) || !length(t)) return("the whole plan")
    paste(vapply(names(t), function(n)
      paste0(n, " ", paste(as.character(t[[n]]), collapse = "/")), character(1)),
      collapse = ", ")
  }
  d <- if (!is.null(op$during))
    sprintf(" in market %s to %s", op$during$from, op$during$to) else ""

  if (!is.null(op$add)) {
    lg <- setdiff(names(op$add), c("planned_spend", "flight_start", "flight_end"))
    return(sprintf("Add %s at %s",
                   paste(unlist(op$add[lg]), collapse = " / "),
                   money(op$add$planned_spend)))
  }
  if (isTRUE(op$drop))     return(sprintf("Drop %s%s", where(op$target), d))
  if (!is.null(op$shift))  return(sprintf("Shift %s by %s days%s", where(op$target), op$shift, d))
  if (!is.null(op$restage))
    return(sprintf("Restage %s to %s - %s", where(op$target), op$restage$from, op$restage$to))
  if (!is.null(op$set))    return(sprintf("Set %s%s to %s", where(op$target), d, money(op$set)))
  if (!is.null(op$total))  return(sprintf("Make %s%s total %s", where(op$target), d, money(op$total)))
  if (!is.null(op$delta))  return(sprintf("Move %s's%s total by %s", where(op$target), d, money(op$delta)))
  if (!is.null(op$delta_each))
    return(sprintf("Add %s to each row of %s%s", money(op$delta_each), where(op$target), d))
  if (!is.null(op$scale))  return(sprintf("Scale %s%s by %s", where(op$target), d, op$scale))
  "Operation"
}

# The editable grid. Spend cells render as number inputs carrying their cell
# key; www/grid.js delegates their change events back to Shiny.
# Cells where two plans disagree, as cell keys. Covers rows that exist in one
# and not the other, so an added or dropped line item highlights too.
changed_cells <- function(base, other) {
  lg <- setdiff(mediaplanr::line_item_grain(base), mediaplanr::flight_cols())
  wk <- base@week_col
  key <- function(p) {
    d <- p@data
    if (!nrow(d)) return(character(0))
    cell_key(mediaplanr::line_item(d, lg),
             if (length(wk)) as.character(d[[wk]]) else NA)
  }
  kb <- key(base); ko <- key(other)
  vb <- stats::setNames(base@data$planned_spend, kb)
  vo <- stats::setNames(other@data$planned_spend, ko)
  all_k <- union(kb, ko)
  b <- ifelse(is.na(vb[all_k]), 0, vb[all_k])
  o <- ifelse(is.na(vo[all_k]), 0, vo[all_k])
  all_k[b != o]
}

# `plan` is the STAGED plan -- the active plan with the pending ops already
# applied -- so the grid is a straight render of it rather than an overlay.
# `baseline` is the unstaged plan, used only to work out which cells to
# highlight as dirty.
plan_grid <- function(plan, baseline = NULL, editable = TRUE) {
  w <- plan_wide(plan)
  df <- w$df
  lg <- w$li_grain

  df$Total <- rowSums(df[, w$value_cols, drop = FALSE], na.rm = TRUE)

  # Dirty cells are wherever the staged plan differs from the unstaged one.
  # Derived rather than tracked, so an operation touching forty cells lights up
  # all forty without anyone enumerating them.
  edited_keys <- if (is.null(baseline)) character(0) else
    changed_cells(baseline, plan)
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
