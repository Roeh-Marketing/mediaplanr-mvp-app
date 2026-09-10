test_that("money is conserved exactly, across awkward totals and calendars", {
  # Everything here is compared in integer cents. Comparing dollars with
  # expect_equal() would pass on a plan that is quietly a cent light.
  grid <- expand.grid(total = c(2e6, 1e6 / 3, 999999.99, 0),
                      n_weeks = c(1L, 13L, 53L),
                      stringsAsFactors = FALSE)

  for (i in seq_len(nrow(grid))) {
    r  <- demo_recipe(total = grid$total[i], n_weeks = grid$n_weeks[i])
    df <- scaffold_df(r)
    expect_identical(sum(cents(df$planned_spend)), cents(grid$total[i]),
                     info = sprintf("total=%s n_weeks=%s",
                                    grid$total[i], grid$n_weeks[i]))
  }
})

test_that("round_to allocates in its own units rather than rounding afterwards", {
  r <- demo_recipe(total = 1e6)
  r$budget$round_to <- 100
  df <- scaffold_df(r)

  expect_identical(sum(cents(df$planned_spend)), cents(1e6))
  # Every figure is a whole hundred -- which round(x, -2) after the fact could
  # give too, but not while also summing to the stated total.
  expect_true(all(df$planned_spend %% 100 == 0))
})

test_that("every node's subtotal equals its own allocation, not just the leaves", {
  # The leaves can sum correctly while TV is a cent light and Search a cent
  # heavy, so this is a separate assertion from the one above.
  r  <- demo_recipe(total = 1e6 / 3)
  df <- scaffold_df(r)
  tot <- cents(1e6 / 3)

  by_channel <- tapply(cents(df$planned_spend), df$channel, sum)
  expect_identical(sum(as.integer(by_channel)), tot)

  # 55 / 25 / 20 of the total, to the cent, after a three-way split of a
  # number that does not divide.
  expect_identical(as.integer(by_channel[["TV"]]),     largest_remainder(tot, c(0.55, 0.25, 0.20))[1])
  expect_identical(as.integer(by_channel[["Search"]]), largest_remainder(tot, c(0.55, 0.25, 0.20))[2])
  expect_identical(as.integer(by_channel[["Social"]]), largest_remainder(tot, c(0.55, 0.25, 0.20))[3])

  # And one level deeper: NBC/ESPN split exactly what TV was given.
  tv <- as.integer(by_channel[["TV"]])
  nbc <- sum(cents(df$planned_spend[df$partner == "NBC"]))
  esp <- sum(cents(df$planned_spend[df$partner == "ESPN"]))
  expect_identical(nbc + esp, tv)
  expect_identical(c(nbc, esp), largest_remainder(tv, c(0.60, 0.40)))
})

test_that("weight and equal splits divide what their parent got", {
  r   <- demo_recipe(total = 1e6)
  df  <- scaffold_df(r)
  soc <- sum(cents(df$planned_spend[df$channel == "Social"]))

  meta <- sum(cents(df$planned_spend[df$partner == "Meta"]))
  tik  <- sum(cents(df$planned_spend[df$partner == "TikTok"]))
  expect_identical(c(meta, tik), largest_remainder(soc, c(3, 1)))

  # Search has one child under an "equal" rule: it gets all of Search.
  expect_identical(sum(cents(df$planned_spend[df$partner == "Google"])),
                   sum(cents(df$planned_spend[df$channel == "Search"])))
})

test_that("an explicit amount is taken off the top and the rest still splits", {
  r <- demo_recipe(total = 1e6)
  r$tree$children[[1]]$amount <- 500000   # "TV is 500k"
  df <- scaffold_df(r)

  expect_identical(sum(cents(df$planned_spend[df$channel == "TV"])), cents(500000))
  expect_identical(sum(cents(df$planned_spend)), cents(1e6))
  # Search and Social divide the remaining 500k by their own 25/20 weights.
  rest <- sum(cents(df$planned_spend[df$channel != "TV"]))
  expect_identical(rest, cents(500000))
})

test_that("nesting is not a cross product", {
  # The whole reason the feature exists: TV owns NBC and ESPN, Search owns
  # Google. A cartesian would give 6 line items and invent Search|ESPN.
  r  <- demo_recipe()
  df <- scaffold_df(r)
  items <- unique(paste(df$channel, df$partner, sep = " | "))

  expect_setequal(items, c("TV | NBC", "TV | ESPN", "Search | Google",
                           "Social | Meta", "Social | TikTok"))
  expect_length(items, 5L)
  expect_false("Search | ESPN" %in% items)
  expect_identical(nrow(df), 5L * 13L)
})

test_that("the calendar is exactly what was asked for", {
  r  <- demo_recipe(n_weeks = 13L)
  df <- scaffold_df(r)

  expect_length(unique(df$week), 13L)
  expect_length(unique(format(df$week, "%u")), 1L)
  expect_equal(min(df$week), as.Date("2026-07-06"))

  # A mid-week start snaps back to the week containing it, rather than
  # shifting the whole calendar off the week-start weekday.
  r2 <- demo_recipe(); r2$time$start <- "2026-07-09"   # a Thursday
  expect_equal(min(scaffold_df(r2)$week), as.Date("2026-07-06"))

  # Sunday-start plans are a real convention, not an error.
  r3 <- demo_recipe(); r3$time$week_start <- "Sunday"
  expect_identical(unique(format(scaffold_df(r3)$week, "%u")), "7")
})

test_that("a leaf with its own dates runs only inside them", {
  r <- demo_recipe()
  r$tree$children[[3]]$children[[2]]$from <- "2026-08-03"
  r$tree$children[[3]]$children[[2]]$to   <- "2026-09-28"
  df <- scaffold_df(r)

  tik <- df[df$partner == "TikTok", ]
  expect_true(min(tik$week) >= as.Date("2026-08-03"))
  expect_true(max(tik$week) <= as.Date("2026-09-28"))
  expect_lt(nrow(tik), 13L)                       # a real gap, not zero-fill
  expect_identical(sum(cents(df$planned_spend)), cents(2e6))   # still exact
})

test_that("pacing shapes the money without changing how much there is", {
  totals <- vapply(pacing_shapes(), function(s) {
    r <- demo_recipe(total = 1e6)
    r$defaults$pacing <- s
    for (i in seq_along(r$tree$children)) {
      for (j in seq_along(r$tree$children[[i]]$children)) {
        r$tree$children[[i]]$children[[j]]$pacing <- s
      }
    }
    sum(cents(scaffold_df(r)$planned_spend))
  }, integer(1))
  expect_true(all(totals == cents(1e6)))

  r <- demo_recipe(total = 1e6)
  df <- scaffold_df(r)

  # flat is level to within the rounding unit
  nbc <- df$planned_spend[df$partner == "NBC"]
  expect_lt(max(nbc) - min(nbc), 0.02)

  # ramp climbs, front is heavier at the start than at the end
  goo <- df$planned_spend[df$partner == "Google"][order(df$week[df$partner == "Google"])]
  expect_false(is.unsorted(goo))

  esp <- df[df$partner == "ESPN", ]
  esp <- esp[order(esp$week), ]
  expect_gt(sum(esp$planned_spend[1:4]), sum(esp$planned_spend[10:13]))
})

test_that("the generated frame is what the Build page can actually build", {
  # The contract with the other two routes: same shape, same mapping check,
  # same constructor. If this passes, do_build() needs no branch of its own.
  skip_if_not_installed("mediaplanr")

  r  <- demo_recipe(total = 2e6)
  u  <- scaffold_to_uploaded(r)
  d  <- u$defaults
  df <- u$sheets[[1]]

  expect_identical(u$source, "scratch")
  expect_identical(names(u$sheets), "baseline")
  expect_identical(check_mapping(df, d$grain, d$week, d$spend), character(0))

  p <- build_plan(df, grain = d$grain, week = d$week, spend_col = d$spend,
                  name = "probe", nickname = "baseline",
                  status = "in development")
  expect_s3_class(p, "mediaplanr::MediaPlan")
  expect_identical(cents(sum(p@data$planned_spend)), cents(2e6))
  expect_identical(p@grain, c("channel", "partner", "week"))
  expect_identical(mediaplanr::week_start(p), "Monday")
  # Grain uniqueness -- one row per line item per week, which the package
  # requires and a bad generator is exactly how you break.
  expect_identical(anyDuplicated(p@data[, p@grain]), 0L)
})

test_that("the nickname reaches the plan instead of being overridden", {
  # do_build() prefers the sheet name over spec$nickname, so a hard-coded
  # "baseline" key would make the Design tab's nickname field cosmetic.
  r <- demo_recipe()
  r$meta$nickname <- "aggressive TV"
  expect_identical(names(scaffold_to_uploaded(r)$sheets), "aggressive TV")
})

test_that("the flights basis emits one row per buy", {
  skip_if_not_installed("mediaplanr")

  r  <- demo_recipe(total = 1e6, basis = "flights")
  u  <- scaffold_to_uploaded(r)
  df <- u$sheets[[1]]
  d  <- u$defaults

  expect_identical(nrow(df), 5L)
  expect_false("week" %in% names(df))
  expect_identical(d$mode, "flights")
  expect_true(all(df$flight_end >= df$flight_start))
  expect_identical(sum(cents(df$planned_spend)), cents(1e6))

  p <- build_plan_from_flights(df, grain = d$grain, start_col = d$start,
                               end_col = d$end, spend_col = d$spend,
                               week_start = d$week_start, name = "probe",
                               nickname = "baseline", status = "in development")
  expect_identical(cents(sum(mediaplanr::flights(p)$planned_spend)), cents(1e6))
})

test_that("a timeless plan has no time column", {
  r  <- demo_recipe(basis = "none")
  df <- scaffold_df(r)
  expect_identical(nrow(df), 5L)
  expect_false("week" %in% names(df))
  expect_null(scaffold_defaults(r)$week)
})

test_that("problems are named before anything is generated", {
  r <- new_recipe()
  expect_true(any(grepl("no line items", scaffold_problems(r))))

  r <- demo_recipe(); r$time$start <- NULL
  expect_true(any(grepl("first week", scaffold_problems(r))))

  # Shares that do not add up are a mistake worth naming, not something to
  # renormalise behind the planner's back.
  r <- demo_recipe(); r$tree$children[[1]]$alloc <- 0.75
  expect_true(any(grepl("not 100%", scaffold_problems(r))))

  # A leaf that stops short of the last dimension
  r <- demo_recipe()
  r$tree$children[[2]]$children <- NULL
  expect_true(any(grepl("stops short", scaffold_problems(r))))

  # ...and scaffold_df() refuses rather than emitting an NA grain column
  expect_error(scaffold_df(r), "stops short")
})

test_that("a subplan seed is valid, and attaches to the cell it came from", {
  skip_if_not_installed("mediaplanr")

  parent <- build_plan(
    scaffold_df(demo_recipe(total = 2e6)),
    grain = c("channel", "partner", "week"), week = "week",
    spend_col = "planned_spend", name = "Q3 topline", nickname = "baseline",
    status = "in development")

  seed <- subplan_seed_recipe(parent, "TV | NBC")

  # Valid on arrival: the form opens on something that would attach.
  expect_identical(scaffold_problems(seed), character(0))
  expect_identical(seed$dimensions, c("channel", "partner"))

  # It carries the cell's own money and the parent's calendar.
  cell <- parent@data[mediaplanr::line_item(parent@data, c("channel","partner")) == "TV | NBC", ]
  expect_identical(cents(seed$budget$total), cents(sum(cell$planned_spend)))
  expect_identical(seed$time$n_weeks, length(unique(cell$week)))
  expect_equal(as.Date(seed$time$start), min(cell$week))

  # One leaf, and it is the cell.
  leaves <- tree_leaves(seed$tree, seed$dimensions)
  expect_identical(nrow(leaves), 1L)
  expect_identical(leaves$.path, "TV | NBC")

  # And it really does attach, which is the only test that matters.
  sub <- build_from_recipe(seed)
  attached <- mediaplanr::attach_subplan(parent, sub)
  expect_identical(names(attached@subplans), "TV | NBC")
  expect_true(mediaplanr::is_topline(attached))
})

test_that("a seed refined with a finer dimension still attaches", {
  skip_if_not_installed("mediaplanr")

  parent <- build_plan(
    scaffold_df(demo_recipe(total = 2e6)),
    grain = c("channel", "partner", "week"), week = "week",
    spend_col = "planned_spend", name = "Q3 topline", nickname = "baseline",
    status = "in development")

  seed <- subplan_seed_recipe(parent, "Search | Google")
  # The actual work: add a level and split the cell beneath it.
  seed$dimensions <- c("channel", "partner", "daypart")
  seed$tree <- parse_tree_text(
    "Search 100%\n  Google 100%\n    Daytime 60%\n    Evening 40%",
    seed$dimensions)$tree

  expect_identical(scaffold_problems(seed), character(0))
  sub <- build_from_recipe(seed)
  attached <- mediaplanr::attach_subplan(parent, sub)
  expect_identical(names(attached@subplans), "Search | Google")

  # Attaching replaces the parent's rows for that cell with the rollup, so the
  # topline total is unchanged when the subplan spends what the cell did.
  expect_identical(cents(sum(attached@data$planned_spend)), cents(2e6))
})

test_that("a cell that is not a cell is refused by name", {
  skip_if_not_installed("mediaplanr")
  parent <- build_plan(
    scaffold_df(demo_recipe(total = 1e6)),
    grain = c("channel", "partner", "week"), week = "week",
    spend_col = "planned_spend", name = "p", nickname = "b",
    status = "in development")
  expect_error(subplan_seed_recipe(parent, "TV"), "does not name a cell")
  expect_error(subplan_seed_recipe(parent, "TV | Hulu"), "no rows in the plan")
})
