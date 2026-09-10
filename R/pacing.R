# ---------------------------------------------------------------------------
# Pacing shapes.
#
# How a line item's spend is profiled across its periods, as one multiplier per
# period. Lifted out of the sample generator because the scaffold route needs
# exactly the same vocabulary -- a planner seeding a new plan reaches for "ramp
# it up" or "front-load it" long before they reach for a spreadsheet.
#
# The shapes are proportional rather than pinned to fixed week counts, so a
# 26-week plan gets a real ramp instead of the 13-week one padded out. At
# n = 13 they reproduce the original sample exactly, which is what keeps
# data/sample_media_plan.csv stable across this refactor.
# ---------------------------------------------------------------------------

pacing_shapes <- function() c("flat", "ramp", "front", "back", "burst")

pacing_curve <- function(shape, n) {
  n <- as.integer(n)
  if (is.na(n) || n < 1L) stop("`n` must be at least 1.", call. = FALSE)
  if (!shape %in% pacing_shapes()) {
    stop("unknown pacing shape '", shape, "'; one of ",
         paste(pacing_shapes(), collapse = ", "), ".", call. = FALSE)
  }
  switch(shape,
    flat = rep(1, n),
    ramp = seq(0.55, 1.45, length.out = n),

    # Heavy for the first ~30%, level for the next ~30%, light for the rest.
    front = {
      a <- max(1L, round(0.3 * n))
      b <- max(1L, round(0.3 * n))
      c(rep(1.5, a), rep(1.0, b), rep(0.4, max(0L, n - a - b)))[seq_len(n)]
    },

    # Quiet for the first ~40%, then climbing to the end.
    back = {
      a <- max(1L, round(0.4 * n))
      c(rep(0.3, a), seq(0.8, 1.6, length.out = max(1L, n - a)))[seq_len(n)]
    },

    # Two-period bursts, roughly one every four periods.
    burst = {
      k  <- max(1L, floor(n / 4))
      s  <- ceiling(seq(1, max(1, n - 1), length.out = k))
      on <- unique(pmin(c(s, s + 1L), n))
      ifelse(seq_len(n) %in% on, 1.8, 0.25)
    })
}
