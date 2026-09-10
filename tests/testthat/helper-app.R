# scaffold.R and pacing.R are deliberately Shiny-free, which is what lets them
# be sourced and tested without a running app. Keep them that way.
source("../../R/app_helpers.R")   # %||%, guess_cols
source("../../R/pacing.R")
source("../../R/scaffold.R")
source("../../R/plan_io.R")

# A three-level recipe with every split rule in play, used across the suite.
demo_recipe <- function(total = 2e6, basis = "weekly", n_weeks = 13L) {
  r <- new_recipe()
  r$meta$name  <- "Q3 2026 Launch"
  r$time       <- list(basis = basis, week_start = "Monday",
                       start = "2026-07-06", n_weeks = n_weeks)
  r$dimensions <- c("channel", "partner")
  r$budget     <- list(total = total, round_to = 0.01)
  r$tree <- list(split = "share", children = list(
    node("TV", alloc = 0.55, split = "share", children = list(
      node("NBC",  alloc = 0.60, pacing = "flat"),
      node("ESPN", alloc = 0.40, pacing = "front"))),
    node("Search", alloc = 0.25, split = "equal", children = list(
      node("Google", pacing = "ramp"))),
    node("Social", alloc = 0.20, split = "weight", children = list(
      node("Meta",   alloc = 3, pacing = "ramp"),
      node("TikTok", alloc = 1, pacing = "back")))
  ))
  r
}

cents <- function(x) as.integer(round(x * 100))
