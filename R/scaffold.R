# ---------------------------------------------------------------------------
# Designing a plan from nothing.
#
# The other two Build routes start from data: a sample knows its own columns, a
# file is asked about its own columns. This one starts from a RECIPE -- a plain
# list saying what the grain should be, over what weeks, funded by what budget
# -- and generates the data frame the other two routes upload.
#
# Deliberately free of Shiny. Nothing here may touch `input`, `req` or
# `showNotification`: that is what lets the whole generator be tested without a
# running app, and the generator is the one new component that can emit a plan
# that is wrong but perfectly valid.
#
# The grain is a TREE, not a cross product. Each value carries its own children
# -- TV owns NBC and ESPN, Search owns Google -- so designing a plan never
# invents the line items nobody buys.
# ---------------------------------------------------------------------------

# `%||%` comes from app_helpers.R, which treats length-0 as absent too --
# relied on here, since a cleared text input arrives as character(0).

PATH_SEP <- " | "   # matches mediaplanr::line_item()'s default separator

# The dimension names the app OFFERS. Suggestions only, never a taxonomy: the
# package hard-codes no column name anywhere ("channel" is simply a dimension
# someone chose to key on), so any name the user types is equally valid.
DIMENSION_SUGGESTIONS <- c(
  "channel", "partner", "campaign", "tactic", "format", "target_audience",
  "market", "brand", "product", "daypart", "placement", "creative"
)

# How a node's children divide what it was given.
split_rules <- function() c("equal", "weight", "share")

time_bases <- function() c("weekly", "flights", "none")

# ---------------------------------------------------------------------------
# Money
#
# Every allocation runs in integer MINOR UNITS and is split level by level.
#
# The tempting shortcut -- multiply the shares down a path and round once at the
# leaf -- is wrong, and wrong in a way nobody notices: at three levels the cents
# do not cancel, so a stated 2,000,000 budget reads 1,999,999.94 and every
# subtotal above it is quietly off. Splitting an integer at each level makes the
# leaves sum to the total exactly AND every intermediate node equal its own
# stated share.
#
# Same discipline, and the same largest-remainder rule, as the package's own
# .allocate_minor() in mediaplanr/R/flighting.R -- which is package-internal and
# so cannot be reused here. If it is ever exported, delete this.
# ---------------------------------------------------------------------------

largest_remainder <- function(total_units, weights) {
  w <- as.numeric(weights)
  if (!length(w)) return(integer(0))
  w[!is.finite(w) | w < 0] <- 0
  if (sum(w) == 0) w <- rep(1, length(w))

  total_units <- as.numeric(total_units)
  raw  <- total_units * w / sum(w)
  base <- floor(raw)
  rem  <- as.integer(round(total_units - sum(base)))
  if (rem > 0L) {
    # Ties to the earliest sibling, so the answer does not depend on how a
    # caller happened to sort the children.
    ord <- order(raw - base, seq_along(raw),
                 decreasing = c(TRUE, FALSE), method = "radix")[seq_len(rem)]
    base[ord] <- base[ord] + 1
  }
  as.integer(base)
}

# ---------------------------------------------------------------------------
# The tree
#
# A node is list(name=, alloc=, split=, pacing=, from=, to=, amount=, children=).
# No children means a leaf. Children are an ORDERED LIST, never a named list
# keyed by value: JSON objects do not promise key order, and dimension values
# are arbitrary user strings that may collide with list semantics.
# ---------------------------------------------------------------------------

node <- function(name, alloc = NULL, split = "equal", pacing = NULL,
                 from = NULL, to = NULL, amount = NULL, children = NULL) {
  out <- list(name = name, alloc = alloc, split = split, pacing = pacing,
              from = from, to = to, amount = amount)
  if (!is.null(children)) out$children <- children
  out
}

is_leaf <- function(nd) !length(nd$children %||% list())

# Depth-first walk to one row per leaf. A leaf's dimension columns are its
# ancestors' names, positionally -- which is why a leaf that stops short of the
# last dimension is an error rather than an NA fill: an NA channel is not a
# plan, it is a missing answer.
tree_leaves <- function(tree, dimensions) {
  rows <- list()

  walk <- function(nd, path_names) {
    if (is_leaf(nd)) {
      rows[[length(rows) + 1L]] <<- list(
        names  = path_names,
        pacing = nd$pacing,
        from   = nd$from,
        to     = nd$to
      )
      return(invisible(NULL))
    }
    for (kid in nd$children) walk(kid, c(path_names, kid$name))
  }
  for (kid in tree$children %||% list()) walk(kid, kid$name)

  if (!length(rows)) {
    return(data.frame(.path = character(0), stringsAsFactors = FALSE))
  }

  out <- do.call(rbind, lapply(rows, function(r) {
    nm <- r$names
    length(nm) <- length(dimensions)          # pads short paths with NA
    d <- as.data.frame(as.list(stats::setNames(nm, dimensions)),
                       stringsAsFactors = FALSE)
    d$.path  <- paste(r$names, collapse = PATH_SEP)
    d$pacing <- r$pacing %||% NA_character_
    d$from   <- if (is.null(r$from)) as.Date(NA) else as.Date(r$from)
    d$to     <- if (is.null(r$to))   as.Date(NA) else as.Date(r$to)
    d
  }))
  rownames(out) <- NULL
  out
}

# Split `units` down the tree, returning integer minor units keyed by leaf path.
#
# A child carrying an explicit `amount` is taken off the top -- that is the one
# door an absolute figure can enter by, and it exists so a planner's own "TV is
# 500k" is expressible. Everything else is divided by the node's own rule.
allocate_tree <- function(tree, units, unit = 0.01) {
  out <- integer(0)

  descend <- function(nd, nd_units, path_names) {
    if (is_leaf(nd)) {
      key <- paste(path_names, collapse = PATH_SEP)
      out[[key]] <<- as.integer(nd_units)
      return(invisible(NULL))
    }
    kids  <- nd$children
    fixed <- vapply(kids, function(k) {
      if (is.null(k$amount)) NA_real_ else as.numeric(k$amount)
    }, numeric(1))

    given <- rep(0L, length(kids))
    has   <- !is.na(fixed)
    given[has] <- as.integer(round(fixed[has] / unit))

    pool <- nd_units - sum(given)
    if (pool < 0) pool <- 0L      # over-subscription is caught by the validator

    free <- which(!has)
    if (length(free)) {
      rule <- nd$split %||% "equal"
      w <- if (identical(rule, "equal")) {
        rep(1, length(free))
      } else {
        vapply(kids[free], function(k) as.numeric(k$alloc %||% 0), numeric(1))
      }
      given[free] <- largest_remainder(pool, w)
    }

    for (i in seq_along(kids)) {
      descend(kids[[i]], given[i], c(path_names, kids[[i]]$name))
    }
  }

  descend(tree, as.integer(units), character(0))
  out
}

# ---------------------------------------------------------------------------
# Calendar
# ---------------------------------------------------------------------------

weekday_num <- function(x) {
  nms <- c("monday", "tuesday", "wednesday", "thursday", "friday",
           "saturday", "sunday")
  if (is.numeric(x)) return(as.integer(x[1]))
  i <- pmatch(tolower(trimws(as.character(x)[1])), nms)
  if (is.na(i)) stop("unknown weekday '", x, "'.", call. = FALSE)
  as.integer(i)
}

# Snap a date back to the start of the week it falls in. A planner typing a
# mid-week start date means "the week containing this", not "shift my whole
# calendar by three days".
week_floor_date <- function(d, week_start = "Monday") {
  wd <- weekday_num(week_start)
  d <- as.Date(d)
  d - ((as.integer(format(d, "%u")) - wd) %% 7L)
}

recipe_weeks <- function(recipe) {
  tm <- recipe$time
  seq(week_floor_date(tm$start, tm$week_start %||% "Monday"),
      by = "week", length.out = as.integer(tm$n_weeks))
}

# ---------------------------------------------------------------------------
# The recipe
# ---------------------------------------------------------------------------

new_recipe <- function() {
  list(
    version    = 1L,
    meta       = list(name = "", nickname = "baseline", advertiser = "",
                      planner = "", objective = ""),
    time       = list(basis = "weekly", week_start = "Monday",
                      start = NULL, n_weeks = 13L),
    dimensions = c("channel"),
    tree       = list(split = "equal", children = list()),
    measures   = list(unit_type = NULL, rate = NULL),
    budget     = list(total = NULL, round_to = 0.01),
    defaults   = list(pacing = "flat")
  )
}

# What is still missing or contradictory. Returns character(0) when the recipe
# is ready to generate -- the same shape as check_mapping(), so the Build page
# renders it through the machinery it already has.
scaffold_problems <- function(recipe) {
  p <- character(0)
  dims <- recipe$dimensions %||% character(0)
  tm   <- recipe$time %||% list()

  if (!length(dims)) p <- c(p, "Name at least one dimension (channel, partner, ...).")
  if (anyDuplicated(dims)) p <- c(p, "Dimension names must be unique.")

  basis <- tm$basis %||% "weekly"
  if (!basis %in% time_bases()) {
    p <- c(p, paste0("Unknown time basis '", basis, "'."))
  }
  if (basis == "weekly") {
    if (is.null(tm$start)) p <- c(p, "A weekly plan needs a first week.")
    n <- suppressWarnings(as.integer(tm$n_weeks %||% NA))
    if (is.na(n) || n < 1L) p <- c(p, "Number of weeks must be at least 1.")
  }

  leaves <- tree_leaves(recipe$tree %||% list(), dims)
  if (!nrow(leaves)) {
    p <- c(p, "Add some values -- the plan has no line items yet.")
  } else {
    if (length(dims)) {
      short <- leaves[!stats::complete.cases(leaves[, dims, drop = FALSE]), , drop = FALSE]
      if (nrow(short)) {
        p <- c(p, paste0(
          "'", short$.path[1], "' stops short of ", dims[length(dims)],
          ". Give it a value, or drop that dimension."))
      }
    }
    if (anyDuplicated(leaves$.path)) {
      p <- c(p, paste0("Duplicate line item: '",
                       leaves$.path[duplicated(leaves$.path)][1], "'."))
    }
    bad <- setdiff(stats::na.omit(unique(leaves$pacing)), pacing_shapes())
    if (length(bad)) {
      p <- c(p, paste0("Unknown pacing shape '", bad[1], "'; one of ",
                       paste(pacing_shapes(), collapse = ", "), "."))
    }
  }

  total <- recipe$budget$total
  if (!is.null(total) && (!is.numeric(total) || is.na(total) || total < 0)) {
    p <- c(p, "Total budget must be a non-negative number.")
  }

  c(p, share_problems(recipe$tree %||% list(), character(0)))
}

# A "share" node whose children do not sum to 1 is a mistake worth naming, not
# something to silently renormalise -- the planner meant those percentages.
#
# The exception is a node where some sibling carries a fixed `amount`. "TV is
# 500k, then split the rest 25/20 between Search and Social" is a perfectly
# ordinary brief, and those two numbers are plainly relative weights over what
# is left rather than shares of the whole. Demanding they sum to 100% there
# would be demanding arithmetic the planner should not have to do -- and
# allocate_tree() already treats them as weights over the remaining pool, so
# the rule would contradict the behaviour.
share_problems <- function(nd, path) {
  if (is_leaf(nd)) return(character(0))
  p <- character(0)
  if (identical(nd$split %||% "equal", "share")) {
    free  <- Filter(function(k) is.null(k$amount), nd$children)
    fixed <- length(nd$children) - length(free)
    if (length(free) && fixed == 0L) {
      s <- sum(vapply(free, function(k) as.numeric(k$alloc %||% 0), numeric(1)))
      if (abs(s - 1) > 1e-9) {
        where <- if (length(path)) paste0("'", paste(path, collapse = PATH_SEP), "'") else "the top level"
        p <- c(p, sprintf("Shares under %s add up to %.1f%%, not 100%%.", where, s * 100))
      }
    }
  }
  for (k in nd$children) p <- c(p, share_problems(k, c(path, k$name)))
  p
}

# ---------------------------------------------------------------------------
# The generator
# ---------------------------------------------------------------------------

scaffold_df <- function(recipe) {
  probs <- scaffold_problems(recipe)
  if (length(probs)) stop(probs[1], call. = FALSE)

  dims   <- recipe$dimensions
  basis  <- recipe$time$basis %||% "weekly"
  leaves <- tree_leaves(recipe$tree, dims)

  unit  <- recipe$budget$round_to %||% 0.01
  total <- recipe$budget$total %||% 0
  units <- allocate_tree(recipe$tree, round(total / unit), unit = unit)
  leaf_units <- units[leaves$.path]

  if (basis == "none") {
    out <- leaves[, dims, drop = FALSE]
    out$planned_spend <- leaf_units * unit
    return(with_measures(out, recipe))
  }

  if (basis == "flights") {
    weeks <- recipe_weeks(recipe)
    out <- leaves[, dims, drop = FALSE]
    out$flight_start <- ifelse(is.na(leaves$from), min(weeks), leaves$from)
    out$flight_end   <- ifelse(is.na(leaves$to), max(weeks) + 6L, leaves$to)
    out$flight_start <- as.Date(out$flight_start, origin = "1970-01-01")
    out$flight_end   <- as.Date(out$flight_end,   origin = "1970-01-01")
    out$planned_spend <- leaf_units * unit
    return(with_measures(out, recipe))
  }

  weeks   <- recipe_weeks(recipe)
  default <- recipe$defaults$pacing %||% "flat"

  pieces <- lapply(seq_len(nrow(leaves)), function(i) {
    active <- weeks
    if (!is.na(leaves$from[i])) active <- active[active >= week_floor_date(leaves$from[i], recipe$time$week_start %||% "Monday")]
    if (!is.na(leaves$to[i]))   active <- active[active <= leaves$to[i]]
    if (!length(active)) return(NULL)

    shape <- leaves$pacing[i]
    if (is.na(shape)) shape <- default
    w     <- pacing_curve(shape, length(active))
    cents <- largest_remainder(leaf_units[i], w)

    d <- leaves[rep(i, length(active)), dims, drop = FALSE]
    d$week          <- active
    d$planned_spend <- cents * unit
    d
  })

  out <- do.call(rbind, Filter(Negate(is.null), pieces))
  rownames(out) <- NULL
  out <- out[order(out$week, out[[dims[1]]]), , drop = FALSE]
  rownames(out) <- NULL
  with_measures(out, recipe)
}

# Units ride along as constants when the recipe names them: the package solves
# spend / units / rate as one identity, so stating the rate is enough.
with_measures <- function(df, recipe) {
  m <- recipe$measures %||% list()
  if (!is.null(m$unit_type) && nzchar(m$unit_type)) df$unit_type    <- m$unit_type
  if (!is.null(m$rate)      && !is.na(m$rate))      df$planned_rate <- as.numeric(m$rate)
  df
}

# ---------------------------------------------------------------------------
# Handing off to the Build page
#
# The whole point of the recipe is that it produces the SAME shape the sample
# and file routes produce, so do_build() needs no branch of its own. The
# generator knows every column it wrote, so like a sample it ships its own
# mapping and its subtab shows no column pickers at all.
# ---------------------------------------------------------------------------

scaffold_defaults <- function(recipe) {
  dims  <- recipe$dimensions
  basis <- recipe$time$basis %||% "weekly"
  m     <- recipe$measures %||% list()

  base <- list(
    spend      = "planned_spend",
    week_start = recipe$time$week_start %||% "Monday",
    unit_type  = if (!is.null(m$unit_type) && nzchar(m$unit_type)) "unit_type" else NULL,
    units      = NULL,
    rate       = if (!is.null(m$rate) && !is.na(m$rate)) "planned_rate" else NULL
  )

  if (basis == "flights") {
    return(c(base, list(mode = "flights", grain = dims,
                        start = "flight_start", end = "flight_end")))
  }
  if (basis == "none") {
    return(c(base, list(mode = "weekly", grain = dims, week = NULL)))
  }
  c(base, list(mode = "weekly", grain = c(dims, "week"), week = "week"))
}

scaffold_to_uploaded <- function(recipe) {
  nick <- recipe$meta$nickname %||% ""
  if (!nzchar(nick)) nick <- "baseline"
  list(
    name     = recipe$meta$name %||% "",
    sheets   = stats::setNames(list(scaffold_df(recipe)), nick),
    source   = "scratch",
    defaults = scaffold_defaults(recipe)
  )
}

# ---------------------------------------------------------------------------
# The outline text
#
# The tree is authored as an indented outline and lives in ONE Shiny input.
# That is the whole reason for the format: a single textarea has no re-render
# state to lose, the assistant patches it with one updateTextAreaInput(), and
# depth is unbounded. The read-only table underneath is what makes it legible.
#
#   TV 55%
#     NBC 60%
#     ESPN 40% ~front
#   Social 20%
#     Meta x3 ~ramp
#     TikTok x1 ~back 2026-08-03..2026-09-28
#
# A line is: name, then any of -- NN% (share), xN (weight), =NNN (a fixed
# amount), ~shape (pacing), from..to (its own dates). Markers are read from the
# END of the line, so a name may contain spaces: "Trade Desk 25%" is Trade Desk
# at 25%, not a parse error.
#
# A parent's split rule is INFERRED from its children's markers rather than
# stated separately -- all % is a share, all x is a weight, none is an even
# split. One vocabulary, and nothing to keep in sync.
# ---------------------------------------------------------------------------

parse_money <- function(s) {
  s <- tolower(gsub("[,$£€[:space:]]", "", s))
  mult <- 1
  if (grepl("k$", s)) { mult <- 1e3; s <- sub("k$", "", s) }
  else if (grepl("m$", s)) { mult <- 1e6; s <- sub("m$", "", s) }
  suppressWarnings(as.numeric(s)) * mult
}

# One line's markers, stripped from the right. Returns the bare name plus
# whatever was found.
parse_tree_line <- function(line) {
  toks <- strsplit(trimws(line), "[[:space:]]+")[[1]]
  out  <- list(alloc = NULL, kind = NULL, amount = NULL, pacing = NULL,
               from = NULL, to = NULL)

  while (length(toks)) {
    t <- toks[length(toks)]
    if (grepl("^[0-9.]+%$", t)) {
      out$alloc <- suppressWarnings(as.numeric(sub("%$", "", t))) / 100
      out$kind  <- "share"
    } else if (grepl("^x[0-9.]+$", t)) {
      out$alloc <- suppressWarnings(as.numeric(sub("^x", "", t)))
      out$kind  <- "weight"
    } else if (grepl("^=", t)) {
      out$amount <- parse_money(sub("^=", "", t))
    } else if (grepl("^~", t)) {
      out$pacing <- sub("^~", "", t)
    } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}\\.\\.[0-9]{4}-[0-9]{2}-[0-9]{2}$", t)) {
      parts    <- strsplit(t, "\\.\\.")[[1]]
      out$from <- parts[1]
      out$to   <- parts[2]
    } else {
      break                      # not a marker, so the name ends here
    }
    toks <- toks[-length(toks)]
  }

  out$name <- paste(toks, collapse = " ")
  out
}

parse_tree_text <- function(txt, dimensions = character(0)) {
  problems <- character(0)
  lines <- unlist(strsplit(paste(txt, collapse = "\n"), "\n", fixed = TRUE))
  lines <- gsub("\t", "    ", lines)
  keep  <- nzchar(trimws(lines)) & !grepl("^\\s*#", lines)
  lines <- lines[keep]

  if (!length(lines)) {
    return(list(tree = list(split = "equal", children = list()),
                problems = problems))
  }

  indent <- nchar(sub("^([[:space:]]*).*$", "\\1", lines))
  flat   <- vector("list", length(lines))
  depth  <- integer(length(lines))
  stack  <- integer(0)             # indent at each depth, 1-based

  for (i in seq_along(lines)) {
    ind <- indent[i]
    if (!length(stack)) {
      stack <- ind
    } else if (ind > stack[length(stack)]) {
      stack <- c(stack, ind)
    } else {
      while (length(stack) > 1 && ind < stack[length(stack)]) {
        stack <- stack[-length(stack)]
      }
      if (ind != stack[length(stack)]) {
        problems <- c(problems, sprintf(
          "Line %d ('%s') is indented to a level that does not exist above it.",
          i, trimws(lines[i])))
        stack[length(stack)] <- ind
      }
    }
    depth[i] <- length(stack)
    flat[[i]] <- parse_tree_line(lines[i])
    if (!nzchar(flat[[i]]$name)) {
      problems <- c(problems, sprintf("Line %d has markers but no name.", i))
    }
  }

  if (length(dimensions) && max(depth) > length(dimensions)) {
    problems <- c(problems, sprintf(
      "The outline is %d levels deep but there are only %d dimensions (%s).",
      max(depth), length(dimensions), paste(dimensions, collapse = ", ")))
  }

  # Children of line i are the following lines at depth+1, until a line at
  # depth i or shallower closes it.
  kids_of <- function(i, d) {
    out <- integer(0)
    j <- i + 1L
    while (j <= length(lines) && depth[j] > d) {
      if (depth[j] == d + 1L) out <- c(out, j)
      j <- j + 1L
    }
    out
  }

  # A parent's rule follows its children's markers. Mixing them is the one
  # thing that cannot be resolved by guessing, so it is named.
  rule_for <- function(kids) {
    if (!length(kids)) return("equal")
    kinds <- vapply(kids, function(j) flat[[j]]$kind %||% "", character(1))
    free  <- kinds[vapply(kids, function(j) is.null(flat[[j]]$amount), logical(1))]
    free  <- free[nzchar(free)]
    if (!length(free)) return("equal")
    u <- unique(free)
    if (length(u) > 1L) return(NA_character_)
    u
  }

  build <- function(i) {
    kids <- kids_of(i, depth[i])
    rule <- rule_for(kids)
    if (is.na(rule)) {
      problems <<- c(problems, sprintf(
        "'%s' mixes percentages and weights among its children. Use one or the other.",
        flat[[i]]$name))
      rule <- "weight"
    }
    nd <- node(flat[[i]]$name, alloc = flat[[i]]$alloc, split = rule,
               pacing = flat[[i]]$pacing, from = flat[[i]]$from,
               to = flat[[i]]$to, amount = flat[[i]]$amount)
    if (length(kids)) nd$children <- lapply(kids, build)
    nd
  }

  roots <- which(depth == 1L)
  rule  <- rule_for(roots)
  if (is.na(rule)) {
    problems <- c(problems,
                  "The top level mixes percentages and weights. Use one or the other.")
    rule <- "weight"
  }

  list(tree     = list(split = rule, children = lapply(roots, build)),
       problems = problems)
}

# The inverse, so the agent can rewrite the tree and the textarea shows it.
format_tree_text <- function(tree, dimensions = character(0)) {
  lines <- character(0)

  emit <- function(nd, parent_rule, d) {
    bits <- character(0)
    if (!is.null(nd$amount)) {
      bits <- c(bits, paste0("=", format(nd$amount, scientific = FALSE, trim = TRUE)))
    } else if (!is.null(nd$alloc)) {
      bits <- c(bits, switch(parent_rule,
        share  = paste0(format(nd$alloc * 100, trim = TRUE), "%"),
        weight = paste0("x", format(nd$alloc, trim = TRUE)),
        character(0)))
    }
    if (!is.null(nd$pacing)) bits <- c(bits, paste0("~", nd$pacing))
    if (!is.null(nd$from) && !is.null(nd$to)) {
      bits <- c(bits, paste0(nd$from, "..", nd$to))
    }
    lines <<- c(lines, paste0(strrep("  ", d - 1L),
                              paste(c(nd$name, bits), collapse = " ")))
    for (k in nd$children %||% list()) emit(k, nd$split %||% "equal", d + 1L)
  }

  for (k in tree$children %||% list()) emit(k, tree$split %||% "equal", 1L)
  paste(lines, collapse = "\n")
}

# One row per line item, with what it was actually given. This is the table
# under the textarea -- and the thing that shows a planner that TV -> {NBC,
# ESPN} plus Search -> {Google} is three line items, not six.
tree_outline <- function(recipe) {
  dims   <- recipe$dimensions %||% character(0)
  leaves <- tree_leaves(recipe$tree %||% list(), dims)
  if (!nrow(leaves)) return(leaves)

  unit  <- recipe$budget$round_to %||% 0.01
  total <- recipe$budget$total %||% 0
  units <- allocate_tree(recipe$tree, round(total / unit), unit = unit)

  out <- leaves[, intersect(dims, names(leaves)), drop = FALSE]
  out$pacing <- ifelse(is.na(leaves$pacing),
                       recipe$defaults$pacing %||% "flat", leaves$pacing)
  out$amount <- unname(units[leaves$.path]) * unit
  out$share  <- if (sum(out$amount) > 0) out$amount / sum(out$amount) else NA_real_
  out$weeks  <- vapply(seq_len(nrow(leaves)), function(i) {
    if (identical(recipe$time$basis %||% "weekly", "none")) return(NA_integer_)
    w <- recipe_weeks(recipe)
    if (!is.na(leaves$from[i])) w <- w[w >= week_floor_date(leaves$from[i], recipe$time$week_start %||% "Monday")]
    if (!is.na(leaves$to[i]))   w <- w[w <= leaves$to[i]]
    length(w)
  }, integer(1))
  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------------------
# Patching a recipe
#
# The assistant does not rewrite the whole recipe; it sends the part it is
# changing. Objects merge recursively and JSON null deletes a key, so "make it
# 26 weeks" touches nothing else.
#
# `children` is the exception to array-replacement: it merges BY NAME, so
# "add Hulu under TV" does not require resending TV's whole roster, and
# {"name":"ESPN","drop":true} removes one. `drop` is the same word
# apply_edits already uses for removing rows, so there is one deletion idiom in
# the app rather than two.
# ---------------------------------------------------------------------------

merge_scaffold <- function(recipe, patch) {
  if (!is.list(patch)) return(patch)

  for (k in names(patch)) {
    v <- patch[[k]]

    if (is.null(v)) {                      # JSON null deletes
      recipe[[k]] <- NULL
      next
    }
    if (identical(k, "children")) {
      recipe[[k]] <- merge_children(recipe[[k]] %||% list(), v)
      next
    }
    if (is.list(v) && !is.null(names(v)) && is.list(recipe[[k]])) {
      recipe[[k]] <- merge_scaffold(recipe[[k]], v)
      next
    }
    recipe[[k]] <- v                       # scalars and arrays replace
  }
  recipe
}

merge_children <- function(existing, patch) {
  nms <- vapply(existing, function(k) k$name %||% "", character(1))

  for (p in patch) {
    nm <- p$name %||% ""
    i  <- match(nm, nms)
    if (isTRUE(p$drop)) {
      if (!is.na(i)) {
        existing <- existing[-i]
        nms      <- nms[-i]
      }
      next
    }
    if (is.na(i)) {
      p$drop <- NULL
      existing <- c(existing, list(p))
      nms      <- c(nms, nm)
    } else {
      existing[[i]] <- merge_scaffold(existing[[i]], p)
    }
  }
  existing
}

# What the model still has to ask about. Returned alongside every patch so it
# can ask for one thing at a time instead of guessing at the rest.
scaffold_missing <- function(recipe) {
  out <- list()
  add <- function(field, why) out[[length(out) + 1L]] <<- list(field = field, why = why)

  if (!length(recipe$dimensions %||% character(0))) {
    add("dimensions", "Nothing identifies a line item yet.")
  }
  if (identical(recipe$time$basis %||% "weekly", "weekly") &&
      is.null(recipe$time$start)) {
    add("time.start", "A weekly plan needs a first week.")
  }
  if (!length(tree_leaves(recipe$tree %||% list(), recipe$dimensions %||% character(0))$.path)) {
    add("tree", "No values yet, so the plan has no line items.")
  }
  if (is.null(recipe$budget$total)) {
    add("budget.total", "No budget stated, so every line item would be zero.")
  }
  if (!nzchar(recipe$meta$name %||% "")) {
    add("meta.name", "Every plan is named; it is required to build.")
  }
  out
}

# How each node divides what it was given, in words. This is what the model is
# allowed to repeat back -- the RULE, never the resulting figures, which only
# preview_scaffold may report.
scaffold_rules <- function(recipe) {
  out <- list()

  describe <- function(nd, path) {
    if (is_leaf(nd)) return(invisible(NULL))
    rule <- nd$split %||% "equal"
    bits <- vapply(nd$children, function(k) {
      if (!is.null(k$amount)) return(paste0(k$name, " =", k$amount))
      switch(rule,
        share  = paste0(k$name, " ", round((k$alloc %||% 0) * 100), "%"),
        weight = paste0(k$name, " x", k$alloc %||% 0),
        k$name)
    }, character(1))
    key <- if (length(path)) paste(path, collapse = PATH_SEP) else "(top level)"
    out[[key]] <<- paste0(rule, ": ", paste(bits, collapse = ", "))
    for (k in nd$children) describe(k, c(path, k$name))
  }

  describe(recipe$tree %||% list(), character(0))
  out
}

# ---------------------------------------------------------------------------
# Designing a subplan
#
# A subplan is an ordinary plan that happens to refine one cell of another, so
# it is authored through the same Design form. What it cannot do is wander: its
# line item grain must CONTAIN the parent's, and every row must sit in the one
# cell it backs. Both are guaranteed here by seeding the recipe from the parent
# rather than asking the planner to retype the cell and hoping they match.
#
# The seed is deliberately valid the moment it is made -- same grain as the
# parent, the cell's own money, the parent's calendar -- so the form opens on
# something that would attach. Refining it is adding a dimension and splitting
# the cell underneath, which is the actual work.
# ---------------------------------------------------------------------------

subplan_seed_recipe <- function(parent, key) {
  li <- setdiff(mediaplanr::line_item_grain(parent),
                mediaplanr::flight_cols())
  if (!length(li)) {
    stop("the parent's grain is only its time column, so it has no cells to ",
         "refine.", call. = FALSE)
  }
  parts <- strsplit(key, PATH_SEP, fixed = TRUE)[[1]]
  if (length(parts) != length(li)) {
    stop("'", key, "' does not name a cell of this plan (its line items are ",
         paste(li, collapse = " + "), ").", call. = FALSE)
  }

  d    <- parent@data
  rows <- mediaplanr::line_item(d, li) == key
  if (!any(rows)) stop("no rows in the plan for '", key, "'.", call. = FALSE)
  cell <- d[rows, , drop = FALSE]

  r <- new_recipe()
  r$dimensions <- li
  r$meta <- list(name = paste0(parent@name, " - ", key), nickname = key,
                 advertiser = parent@advertiser, planner = parent@planner,
                 objective = paste("Detail plan for", key))
  r$budget <- list(total = sum(cell[["planned_spend"]]), round_to = 0.01)

  wk <- parent@week_col
  if (length(wk) && nrow(cell)) {
    weeks <- sort(unique(cell[[wk]]))
    r$time <- list(basis = "weekly",
                   week_start = mediaplanr::week_start(parent) %||% "Monday",
                   start = as.character(min(weeks)),
                   n_weeks = length(weeks))
  } else {
    r$time <- list(basis = "none", week_start = "Monday", start = NULL,
                   n_weeks = 1L)
  }

  # The cell itself, as a chain: TV, or TV -> NBC. One leaf, taking everything,
  # which is what makes the seed attach cleanly before it is refined.
  node_chain <- function(i) {
    nd <- node(parts[i], alloc = 1, split = "equal")
    if (i < length(parts)) nd$children <- list(node_chain(i + 1L))
    nd
  }
  r$tree <- list(split = "share", children = list(node_chain(1L)))
  r
}

# A recipe straight to a MediaPlan, through the same `defaults` contract the
# Build page reads. One door, so the button, the agent's build tool and a
# subplan being authored cannot each decide the grain differently.
build_from_recipe <- function(recipe, status = "in development") {
  u <- scaffold_to_uploaded(recipe)
  d <- u$defaults
  args <- list(u$sheets[[1]], grain = d$grain, spend_col = d$spend,
               units_col = d$units, rate_col = d$rate,
               unit_type_col = d$unit_type,
               name = recipe$meta$name, nickname = names(u$sheets)[1],
               advertiser = recipe$meta$advertiser %||% "",
               planner = recipe$meta$planner %||% "", status = status)
  if (identical(d$mode, "flights")) {
    do.call(build_plan_from_flights,
            c(args, list(start_col = d$start, end_col = d$end,
                         week_start = d$week_start)))
  } else {
    do.call(build_plan, c(args, list(week = d$week)))
  }
}
