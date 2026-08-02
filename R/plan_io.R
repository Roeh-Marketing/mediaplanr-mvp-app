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

# Returns list(name = <plan name>, sheets = named list of data frames).
# A csv yields a single unnamed sheet; an xlsx yields one per worksheet.
read_plan_file <- function(path, filename = basename(path)) {
  ext <- tolower(tools::file_ext(filename))
  nm  <- plan_name_from_file(filename)

  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(path)
    frames <- lapply(sheets, function(s) {
      as.data.frame(readxl::read_excel(path, sheet = s), stringsAsFactors = FALSE)
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
    return(list(name = nm, sheets = stats::setNames(list(df), "")))
  }

  stop("Unsupported file type '", ext, "'. Upload a .csv, .tsv or .xlsx.",
       call. = FALSE)
}

# Build one MediaPlan from a mapped data frame. Thin over
# mediaplanr::media_plan_from_df(); its errors are already user-readable, so
# they are allowed to propagate to the notification.
build_plan <- function(df, grain, week = NULL, spend_col = "planned_spend",
                       name, nickname = "", advertiser = "", planner = "",
                       status = "in development", objective = "") {
  mediaplanr::media_plan_from_df(
    df,
    grain         = grain,
    week          = week,
    planned_spend = spend_col,
    name          = name,
    nickname      = nickname,
    advertiser    = advertiser,
    planner       = planner,
    status        = status,
    objective     = objective
  )
}

# Validate a mapping against a data frame BEFORE constructing, so the Plan page
# can show what is wrong while the user is still choosing columns rather than
# only failing at the moment they click Build.
check_mapping <- function(df, grain, week, spend_col) {
  problems <- character(0)

  if (!length(grain)) {
    problems <- c(problems, "Pick at least one grain column.")
  }
  miss <- setdiff(c(grain, week, spend_col), c(names(df), NULL))
  miss <- miss[nzchar(miss)]
  if (length(miss)) {
    problems <- c(problems, paste0("Column(s) not in the file: ",
                                   paste(miss, collapse = ", "), "."))
  }
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
