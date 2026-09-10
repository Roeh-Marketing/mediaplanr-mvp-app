test_that("the shapes reproduce the original 13-week sample exactly", {
  # pacing_curve() was lifted out of sample_plan_df(), where the shapes were
  # hard-wired to 13 weeks. data/sample_media_plan.csv was written by that
  # original, so it is the regression fixture: if these drift, the on-disk
  # sample and the generator disagree and nobody finds out.
  n <- 13
  expect_equal(pacing_curve("flat",  n), rep(1, n))
  expect_equal(pacing_curve("ramp",  n), seq(0.55, 1.45, length.out = n))
  expect_equal(pacing_curve("front", n), c(rep(1.5, 4), rep(1.0, 4), rep(0.4, 5)))
  expect_equal(pacing_curve("back",  n), c(rep(0.3, 5), seq(0.8, 1.6, length.out = 8)))
  expect_equal(pacing_curve("burst", n),
               ifelse(seq_len(n) %in% c(1, 2, 7, 8, 12, 13), 1.8, 0.25))
})

test_that("every shape survives every plan length", {
  # The original curve() errored below 8 weeks (front) and 6 (back), and
  # hard-coded burst weeks that only exist in a 13-week plan. A designed plan
  # can be any length, so short calendars are the case that matters.
  for (n in c(1L, 2L, 3L, 4L, 5L, 8L, 26L, 52L)) {
    for (s in pacing_shapes()) {
      v <- pacing_curve(s, n)
      expect_length(v, n)
      expect_false(anyNA(v), info = paste(s, n))
      expect_true(all(v > 0), info = paste(s, n))
    }
  }
})

test_that("each shape does what its name says", {
  expect_false(is.unsorted(pacing_curve("ramp", 26)))
  expect_true(all(pacing_curve("flat", 26) == 1))

  f <- pacing_curve("front", 26)
  expect_gt(sum(f[1:8]), sum(f[19:26]))

  b <- pacing_curve("back", 26)
  expect_lt(sum(b[1:8]), sum(b[19:26]))

  # burst alternates: some periods heavy, most light, never uniform
  br <- pacing_curve("burst", 26)
  expect_gt(length(unique(br)), 1L)
})

test_that("unknown shapes and lengths are refused by name", {
  expect_error(pacing_curve("swoosh", 13), "unknown pacing shape")
  expect_error(pacing_curve("flat", 0), "at least 1")
})
