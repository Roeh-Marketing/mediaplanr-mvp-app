# ---------------------------------------------------------------------------
# The built-in demo plan.
#
# A Q2 2026 weekly plan at channel + partner + week grain: 8 line items across
# 4 channels, 13 weeks. Shaped like a plan a planner would actually hand over --
# uneven flighting, a partner that only runs in part of the quarter, and one
# channel that ramps -- so the comparison charts have something to show.
# ---------------------------------------------------------------------------

sample_plan_df <- function(seed = 42) {
  set.seed(seed)
  weeks <- seq(as.Date("2026-04-06"), by = "week", length.out = 13)

  # line item -> (weekly base spend, shape over the quarter)
  spec <- list(
    list(channel = "TV",      partner = "NBC",      base = 42000, shape = "flat"),
    list(channel = "TV",      partner = "ESPN",     base = 28000, shape = "front"),
    list(channel = "Search",  partner = "Google",   base = 19000, shape = "ramp"),
    list(channel = "Search",  partner = "Bing",     base =  6500, shape = "flat"),
    list(channel = "Social",  partner = "Meta",     base = 23000, shape = "ramp"),
    list(channel = "Social",  partner = "TikTok",   base = 11000, shape = "back"),
    list(channel = "Audio",   partner = "Spotify",  base =  8000, shape = "flat"),
    list(channel = "Display", partner = "Trade Desk", base = 9500, shape = "burst")
  )

  n <- length(weeks)
  curve <- function(shape) {
    switch(shape,
      flat  = rep(1, n),
      ramp  = seq(0.55, 1.45, length.out = n),
      front = c(rep(1.5, 4), rep(1.0, 4), rep(0.4, n - 8)),
      back  = c(rep(0.3, 5), seq(0.8, 1.6, length.out = n - 5)),
      burst = ifelse(seq_len(n) %in% c(1, 2, 7, 8, 12, 13), 1.8, 0.25)
    )
  }

  rows <- lapply(spec, function(s) {
    mult <- curve(s$shape) * stats::runif(n, 0.92, 1.08)
    data.frame(
      week          = weeks,
      channel       = s$channel,
      partner       = s$partner,
      planned_spend = round(s$base * mult, -2),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out[order(out$week, out$channel, out$partner), ]
}

# Written to data/sample_media_plan.csv so the upload path can be exercised too.
write_sample_csv <- function(path = "data/sample_media_plan.csv") {
  utils::write.csv(sample_plan_df(), path, row.names = FALSE)
  invisible(path)
}

# ---------------------------------------------------------------------------
# A second demo plan, authored as FLIGHTS rather than weeks.
#
# The weekly sample above is perfectly dense -- 8 line items x 13 weeks, every
# cell filled -- which is why nothing in the app ever had to confront a sparse
# grid, a flight, or a unit type. This one is deliberately awkward in the ways
# real buys are:
#
#   * OOH starts mid-week and straddles a month end, so calendarize("month")
#     has something to split and the first weekly row is a part week.
#   * TV runs two non-adjacent bursts -- the same line item with two flights,
#     which is what the "one period per line item per week" rule is about.
#   * Search is a single dated insertion: one day, not one week.
#   * Social and Search carry units, so the unit mapping and CPM/CPC handling
#     have something to chew on. TV deliberately carries none.
# ---------------------------------------------------------------------------
sample_flights_df <- function() {
  data.frame(
    channel      = c("OOH",     "TV",     "TV",     "Search", "Social"),
    partner      = c("JCDecaux","NBC",    "NBC",    "Google", "Meta"),
    campaign     = c("Brand",   "Burst 1","Burst 2","Always on","Brand"),
    flight_start = as.Date(c("2026-04-08", "2026-04-06", "2026-05-18",
                             "2026-04-15", "2026-04-06")),
    flight_end   = as.Date(c("2026-05-10", "2026-04-26", "2026-06-14",
                             "2026-04-15", "2026-06-28")),
    planned_spend = c(210000, 180000, 145000, 3100, 140000),
    unit_type     = c(NA, NA, NA, "click", "impression"),
    planned_rate  = c(NA, NA, NA, 0.80, 5),
    stringsAsFactors = FALSE
  )
}

write_sample_flights_csv <- function(path = "data/sample_flight_plan.csv") {
  utils::write.csv(sample_flights_df(), path, row.names = FALSE)
  invisible(path)
}
