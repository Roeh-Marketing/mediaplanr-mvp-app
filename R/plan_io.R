# ---------------------------------------------------------------------------
# Reading plans in.
#
# Two shapes are supported:
#   * a flat csv/tsv -- one plan
#   * an xlsx workbook -- ONE SHEET PER SCENARIO, which is the convention
#     mediaplanr is designed around: the filename becomes the formal @name
#     (shared by every scenario) and each sheet name becomes a @nickname.
#
# Reading stops at "here are the data frames". Turning them into MediaPlans
# needs the column mapping, which the user supplies on the Plan page.
# ---------------------------------------------------------------------------

# A plan name derived from a filename: drop the extension, turn separators into
# spaces, and leave the user's capitalisation alone.
plan_name_from_file <- function(filename) {
  base <- tools::file_path_sans_ext(basename(filename))
  base <- gsub("[_-]+", " ", base)
  trimws(gsub("\\s+", " ", base))
}

# Put mediaplanr's reserved columns back into the types its validator demands.
#
# This is what makes the export -> re-upload loop work for a flighted plan.
# media_plan_from_df() coerces only the WEEK column, so flight_start and
# flight_end arrive as character from a csv (or POSIXct from readxl) and the
# validator rejects the plan outright: "'flight_start' must hold Date values."
#
# period_basis and pacing are checked against their level sets without
# lower-casing, so a workbook where someone typed "Even" or "Week" in Excel
# also fails. media_plan_from_flights() normalises those on its own path; this
# is the same courtesy for the upload path.
normalise_reserved <- function(df) {
  for (nm in intersect(c("flight_start", "flight_end"), names(df))) {
    df[[nm]] <- to_date(df[[nm]])
  }
  for (nm in intersect(c("period_basis", "pacing", "unit_type"), names(df))) {
    if (is.character(df[[nm]]) || is.factor(df[[nm]])) {
      df[[nm]] <- tolower(trimws(as.character(df[[nm]])))
    }
  }
  # readxl hands back every date cell as POSIXct; a plain Date is what the rest
  # of the app and the package expect.
  for (nm in names(df)) {
    if (inherits(df[[nm]], "POSIXct")) df[[nm]] <- as.Date(df[[nm]])
  }
  df
}

# as.Date() errors on unparseable strings rather than returning NA, so guard it
# the same way mediaplanr does. Returns the input untouched when it cannot be
# read as dates, so the mapping check can report the problem in context.
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  out <- suppressWarnings(tryCatch(as.Date(as.character(x)),
                                   error = function(e) NULL))
  if (is.null(out) || all(is.na(out))) x else out
}

# Returns list(name = <plan name>, sheets = named list of data frames).
# A csv yields a single unnamed sheet; an xlsx yields one per worksheet.
read_plan_file <- function(path, filename = basename(path)) {
  ext <- tolower(tools::file_ext(filename))
  nm  <- plan_name_from_file(filename)

  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(path)
    frames <- lapply(sheets, function(s) {
      normalise_reserved(
        as.data.frame(readxl::read_excel(path, sheet = s), stringsAsFactors = FALSE))
    })
    names(frames) <- sheets
    return(list(name = nm, sheets = frames))
  }

  if (ext %in% c("csv", "tsv", "txt")) {
    df <- if (ext == "tsv") {
      utils::read.delim(path, stringsAsFactors = FALSE, check.names = TRUE)
    } else {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
    }
    return(list(name = nm, sheets = stats::setNames(list(normalise_reserved(df)), "")))
  }

  stop("Unsupported file type '", ext, "'. Upload a .csv, .tsv or .xlsx.",
       call. = FALSE)
}

# Build one MediaPlan from a mapped data frame. Thin over
# mediaplanr::media_plan_from_df(); its errors are already user-readable, so
# they are allowed to propagate to the notification.
build_plan <- function(df, grain, week = NULL, spend_col = "planned_spend",
                       units_col = NULL, rate_col = NULL, unit_type_col = NULL,
                       name, nickname = "", advertiser = "", planner = "",
                       status = "in development", objective = "") {
  blank <- function(x) if (is.null(x) || !nzchar(x)) NULL else x
  mediaplanr::media_plan_from_df(
    df,
    grain         = grain,
    week          = week,
    planned_spend = spend_col,
    planned_units = blank(units_col),
    planned_rate  = blank(rate_col),
    unit_type     = blank(unit_type_col),
    name          = name,
    nickname      = nickname,
    advertiser    = advertiser,
    planner       = planner,
    status        = status,
    objective     = objective
  )
}

# Build one MediaPlan from a file of FLIGHTS -- one row per buy, with in-market
# dates instead of a week. mediaplanr expands each buy across the weeks it
# touches, so what comes back is an ordinary weekly plan that the grid, the
# charts and every op already understand.
#
# `grain` here is the LINE ITEM only: the week column is created by the
# constructor, and it errors if the file already has one.
build_plan_from_flights <- function(df, grain, start_col, end_col,
                                    spend_col = "planned_spend",
                                    week_start = "Monday",
                                    units_col = NULL, rate_col = NULL,
                                    unit_type_col = NULL,
                                    name, nickname = "", advertiser = "",
                                    planner = "", status = "in development",
                                    objective = "") {
  blank <- function(x) if (is.null(x) || !nzchar(x)) NULL else x
  df <- .rename_for_units(df, blank(units_col), blank(rate_col),
                          blank(unit_type_col))
  mediaplanr::media_plan_from_flights(
    df,
    grain         = grain,
    start         = start_col,
    end           = end_col,
    planned_spend = spend_col,
    week_start    = week_start,
    name          = name,
    nickname      = nickname,
    advertiser    = advertiser,
    planner       = planner,
    status        = status,
    objective     = objective
  )
}

# media_plan_from_flights() has no planned_units/planned_rate/unit_type
# arguments -- it takes them as canonical columns -- so a mapping is applied by
# renaming here first.
.rename_for_units <- function(df, units_col, rate_col, unit_type_col) {
  ren <- c(planned_units = units_col, planned_rate = rate_col,
           unit_type = unit_type_col)
  for (to in names(ren)) {
    from <- ren[[to]]
    if (is.null(from) || identical(from, to)) next
    if (!from %in% names(df)) next
    if (to %in% names(df)) df[[to]] <- NULL
    names(df)[names(df) == from] <- to
  }
  df
}

# Validate a mapping against a data frame BEFORE constructing, so the Plan page
# can show what is wrong while the user is still choosing columns rather than
# only failing at the moment they click Build.
check_mapping <- function(df, grain, week, spend_col,
                         units_col = NULL, rate_col = NULL) {
  problems <- .check_unit_trio(df, spend_col, units_col, rate_col)

  if (!length(grain)) {
    problems <- c(problems, "Pick at least one grain column.")
  }
  miss <- setdiff(c(grain, week, spend_col), c(names(df), NULL))
  miss <- miss[nzchar(miss)]
  if (length(miss)) {
    problems <- c(problems, paste0("Column(s) not in the file: ",
                                   paste(miss, collapse = ", "), "."))
  }
  problems <- c(problems, .check_trio_arity(spend_col, units_col, rate_col))
  if (nzchar(spend_col %||% "") && spend_col %in% names(df)) {
    v <- suppressWarnings(as.numeric(df[[spend_col]]))
    if (all(is.na(v))) {
      problems <- c(problems, paste0("'", spend_col, "' is not numeric."))
    } else if (any(v < 0, na.rm = TRUE)) {
      problems <- c(problems, paste0("'", spend_col, "' has negative values."))
    } else if (any(is.na(v))) {
      problems <- c(problems, paste0("'", spend_col, "' has missing values."))
    }
  }
  if (!is.null(week) && nzchar(week) && week %in% names(df)) {
    if (!is_dateish(df[[week]])) {
      problems <- c(problems,
                    paste0("'", week, "' does not look like a date."))
    }
    if (!week %in% grain) {
      problems <- c(problems, "The week column must be one of the grain columns.")
    }
  }
  if (length(grain) && all(grain %in% names(df))) {
    k <- do.call(paste, c(lapply(grain, function(g) as.character(df[[g]])),
                          list(sep = " | ")))
    if (anyDuplicated(k)) {
      n <- sum(duplicated(k))
      problems <- c(problems, paste0(
        n, " duplicate row(s) at this grain -- add a column (e.g. week) or ",
        "aggregate first."))
    }
  }
  problems
}

# Spend, units and rate are bound by one identity, so any two give the third.
# Checking it here means "map two of the three" is enforced while mapping
# rather than surfacing as a package error on Build.
.check_unit_trio <- function(df, spend_col, units_col, rate_col) {
  problems <- character(0)
  # Arity ("two of the three") is checked by the caller, which knows whether
  # spend is mapped; emitting it here too produced two messages for one fault.
  for (nm in c(units_col, rate_col)) {
    if (is.null(nm) || !nm %in% names(df)) next
    v <- suppressWarnings(as.numeric(df[[nm]]))
    if (all(is.na(v))) {
      problems <- c(problems, paste0("'", nm, "' is not numeric."))
    } else if (any(v < 0, na.rm = TRUE)) {
      problems <- c(problems, paste0("'", nm, "' has negative values."))
    }
  }
  problems
}

# Spend can be left unmapped only when units and a rate are both there, since
# the package computes whichever of the three is missing.
.check_trio_arity <- function(spend_col, units_col, rate_col) {
  if (nzchar(spend_col %||% "")) return(character(0))
  if (!is.null(units_col) && !is.null(rate_col)) return(character(0))
  paste0("Map a planned spend column, or map both units and a rate so spend ",
         "can be computed.")
}

# The flights mapping is a different shape: no week column (the constructor
# creates it) and two dates that must parse and be the right way round.
check_flight_mapping <- function(df, grain, start_col, end_col, spend_col,
                                 units_col = NULL, rate_col = NULL) {
  problems <- c(.check_unit_trio(df, spend_col, units_col, rate_col),
                .check_trio_arity(spend_col, units_col, rate_col))

  if (!length(grain)) {
    problems <- c(problems, "Pick at least one line item column.")
  }
  if (is.null(start_col) || is.null(end_col)) {
    return(c(problems, "Map both a flight start and a flight end column."))
  }
  miss <- setdiff(c(grain, start_col, end_col), names(df))
  if (length(miss)) {
    return(c(problems, paste0("Column(s) not in the file: ",
                              paste(miss, collapse = ", "), ".")))
  }
  if ("week" %in% names(df)) {
    problems <- c(problems, paste0(
      "This file already has a 'week' column. Flight mode creates one, so use ",
      "the weekly mode instead."))
  }
  fs <- to_date(df[[start_col]]); fe <- to_date(df[[end_col]])
  if (!inherits(fs, "Date")) {
    problems <- c(problems, paste0("'", start_col, "' does not look like a date."))
  }
  if (!inherits(fe, "Date")) {
    problems <- c(problems, paste0("'", end_col, "' does not look like a date."))
  }
  if (inherits(fs, "Date") && inherits(fe, "Date")) {
    if (anyNA(fs) || anyNA(fe)) {
      problems <- c(problems, "Every flight needs both a start and an end date.")
    } else if (any(fe < fs)) {
      problems <- c(problems, paste0(sum(fe < fs),
                                     " flight(s) end before they start."))
    }
  }
  if (nzchar(spend_col %||% "") && spend_col %in% names(df)) {
    v <- suppressWarnings(as.numeric(df[[spend_col]]))
    if (all(is.na(v))) {
      problems <- c(problems, paste0("'", spend_col, "' is not numeric."))
    } else if (any(v < 0, na.rm = TRUE)) {
      problems <- c(problems, paste0("'", spend_col, "' has negative values."))
    }
  }
  problems
}

# ---------------------------------------------------------------------------
# Writing plans out.
#
# The multi-sheet workbook is the important one: it is written in exactly the
# shape read_plan_file() reads, so a workbook exported here can be re-uploaded
# and come back as the same set of scenarios. Export and import are two ends of
# one format, not two features.
# ---------------------------------------------------------------------------

# Excel sheet names: <= 31 chars, no : \ / ? * [ ], non-blank, unique.
excel_sheet_name <- function(x, taken = character(0)) {
  nm <- gsub("[:\\\\/?*\\[\\]]", "-", as.character(x))
  nm <- trimws(nm)
  if (!nzchar(nm)) nm <- "sheet"
  if (nchar(nm) > 31) nm <- substr(nm, 1, 31)
  base <- nm
  i <- 2L
  while (nm %in% taken) {
    suffix <- paste0("~", i)
    nm <- paste0(substr(base, 1, 31 - nchar(suffix)), suffix)
    i <- i + 1L
  }
  nm
}

# One sheet per scenario, named by its label. Round-trips through
# read_plan_file().
write_scenarios_xlsx <- function(set, path, scenarios = NULL) {
  nms <- names(set@scenarios)
  if (!is.null(scenarios)) nms <- intersect(nms, scenarios)
  taken <- character(0)
  sheets <- list()
  for (nm in nms) {
    sn <- excel_sheet_name(nm, taken)
    taken <- c(taken, sn)
    sheets[[sn]] <- set@scenarios[[nm]]@data
  }
  writexl::write_xlsx(sheets, path = path)
  invisible(names(sheets))
}

# A single data frame as a one-sheet workbook.
write_df_xlsx <- function(df, path, sheet = "Sheet1") {
  writexl::write_xlsx(stats::setNames(list(df), excel_sheet_name(sheet)), path = path)
  invisible(path)
}
