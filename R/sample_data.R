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
