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
  week  <- pick(c("^week$", "^date$", "week_start", "^wk$", "period"))

  # Anything character/factor with a sane number of distinct values is a
  # plausible grain column.
  cand <- nms[vapply(df, function(c) is.character(c) || is.factor(c), logical(1))]
  cand <- setdiff(cand, c(spend, week))
  grain <- cand[vapply(cand, function(c) {
    k <- length(unique(df[[c]]))
    k > 1 && k <= max(50, nrow(df) / 2)
  }, logical(1))]

  list(spend = spend, week = week, grain = grain)
}

# Is a column safely coercible to Date? Used to decide whether to offer it as
# the week column.
is_dateish <- function(x) {
  if (inherits(x, "Date")) return(TRUE)
  if (!is.character(x) && !is.factor(x)) return(FALSE)
  v <- suppressWarnings(tryCatch(as.Date(as.character(x)),
                                 error = function(e) NA))
  !all(is.na(v))
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
