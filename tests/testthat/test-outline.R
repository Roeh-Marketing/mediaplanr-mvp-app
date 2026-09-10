outline <- "TV 55%
  NBC 60%
  ESPN 40% ~front
Search 25%
  Google ~ramp
Social 20%
  Meta x3 ~ramp
  TikTok x1 ~back 2026-08-03..2026-09-28"

test_that("the outline parses into the tree the recipe expects", {
  res <- parse_tree_text(outline, c("channel", "partner"))
  expect_identical(res$problems, character(0))

  tr <- res$tree
  expect_identical(tr$split, "share")
  expect_length(tr$children, 3L)
  expect_identical(vapply(tr$children, `[[`, character(1), "name"),
                   c("TV", "Search", "Social"))
  expect_equal(tr$children[[1]]$alloc, 0.55)

  # A parent's rule follows its children's markers, and each parent is read
  # independently: TV's children use %, Social's use x.
  expect_identical(tr$children[[1]]$split, "share")
  expect_identical(tr$children[[3]]$split, "weight")
  expect_equal(tr$children[[3]]$children[[1]]$alloc, 3)

  expect_identical(tr$children[[1]]$children[[2]]$pacing, "front")
  expect_identical(tr$children[[3]]$children[[2]]$from, "2026-08-03")
  expect_identical(tr$children[[3]]$children[[2]]$to,   "2026-09-28")

  # Search's single child carries no marker, so an even split
  expect_identical(tr$children[[2]]$split, "equal")
})

test_that("a parsed outline generates the plan it describes", {
  r <- new_recipe()
  r$meta$name  <- "outline"
  r$time       <- list(basis = "weekly", week_start = "Monday",
                       start = "2026-07-06", n_weeks = 13L)
  r$dimensions <- c("channel", "partner")
  r$budget     <- list(total = 2e6, round_to = 0.01)
  r$tree       <- parse_tree_text(outline, r$dimensions)$tree

  expect_identical(scaffold_problems(r), character(0))
  df <- scaffold_df(r)

  expect_identical(sum(cents(df$planned_spend)), cents(2e6))
  expect_setequal(unique(paste(df$channel, df$partner, sep = " | ")),
                  c("TV | NBC", "TV | ESPN", "Search | Google",
                    "Social | Meta", "Social | TikTok"))
  # TikTok's own dates are honoured
  expect_true(min(df$week[df$partner == "TikTok"]) >= as.Date("2026-08-03"))
})

test_that("names may contain spaces, because markers are read from the right", {
  res <- parse_tree_text("Display 40%\n  Trade Desk 100% ~burst", c("channel", "partner"))
  expect_identical(res$problems, character(0))
  leaf <- res$tree$children[[1]]$children[[1]]
  expect_identical(leaf$name, "Trade Desk")
  expect_identical(leaf$pacing, "burst")
})

test_that("a fixed amount is read as an amount, in whatever units it is typed", {
  res <- parse_tree_text("TV =500k\nSearch =1.2m\nSocial =250000", "channel")
  amts <- vapply(res$tree$children, function(k) k$amount, numeric(1))
  expect_equal(amts, c(5e5, 1.2e6, 2.5e5))

  res2 <- parse_tree_text("TV =$500,000", "channel")
  expect_equal(res2$tree$children[[1]]$amount, 5e5)
})

test_that("the outline round-trips through the formatter", {
  # The agent rewrites the tree and the textarea has to show it back, so the
  # two directions must agree or the UI fights the assistant.
  dims <- c("channel", "partner")
  tr   <- parse_tree_text(outline, dims)$tree
  again <- parse_tree_text(format_tree_text(tr, dims), dims)$tree
  expect_equal(again, tr)
})

test_that("blank lines, comments and tabs are tolerated", {
  txt <- "# the brief\n\nTV 60%\n\tNBC 100%\n\nSearch 40%\n\tGoogle 100%\n"
  res <- parse_tree_text(txt, c("channel", "partner"))
  expect_identical(res$problems, character(0))
  expect_length(res$tree$children, 2L)
  expect_identical(res$tree$children[[1]]$children[[1]]$name, "NBC")
})

test_that("any consistent indent works, not just two spaces", {
  four <- parse_tree_text("TV 60%\n    NBC 100%\nSearch 40%\n    Google 100%",
                          c("channel", "partner"))
  two  <- parse_tree_text("TV 60%\n  NBC 100%\nSearch 40%\n  Google 100%",
                          c("channel", "partner"))
  expect_identical(four$problems, character(0))
  expect_equal(four$tree, two$tree)
})

test_that("confusable outlines are named rather than guessed at", {
  # Mixing % and x under one parent has no defensible reading.
  mixed <- parse_tree_text("TV 50%\nSearch x2", "channel")
  expect_true(any(grepl("mixes percentages and weights", mixed$problems)))

  # Deeper than there are dimensions to hold it
  deep <- parse_tree_text("TV 100%\n  NBC 100%\n    Sport 100%", c("channel", "partner"))
  expect_true(any(grepl("levels deep", deep$problems)))

  # An indent that matches no level above it
  ragged <- parse_tree_text("TV 100%\n      NBC 60%\n   ESPN 40%", c("channel", "partner"))
  expect_true(any(grepl("does not exist above it", ragged$problems)))
})

test_that("an empty outline is empty, not an error", {
  res <- parse_tree_text("", "channel")
  expect_identical(res$problems, character(0))
  expect_length(res$tree$children, 0L)
})

test_that("the outline table shows leaves, amounts and week counts", {
  r <- new_recipe()
  r$time       <- list(basis = "weekly", week_start = "Monday",
                       start = "2026-07-06", n_weeks = 13L)
  r$dimensions <- c("channel", "partner")
  r$budget     <- list(total = 2e6, round_to = 0.01)
  r$tree       <- parse_tree_text(outline, r$dimensions)$tree

  o <- tree_outline(r)
  expect_identical(nrow(o), 5L)
  expect_identical(names(o)[1:2], c("channel", "partner"))
  expect_identical(sum(cents(o$amount)), cents(2e6))
  expect_equal(sum(o$share), 1)

  # TikTok runs a shorter window than the rest, and the table says so
  expect_identical(o$weeks[o$partner != "TikTok"], rep(13L, 4L))
  expect_lt(o$weeks[o$partner == "TikTok"], 13L)

  # An unmarked leaf falls back to the recipe's default pacing
  expect_identical(o$pacing[o$partner == "Google"], "ramp")
  expect_identical(o$pacing[o$partner == "NBC"], "flat")
})
