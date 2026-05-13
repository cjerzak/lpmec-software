# Tests for data-calibrated outcome priors

skip_on_cran()

count_matches <- function(pattern, x) {
  matches <- gregexpr(pattern, x, perl = TRUE)[[1]]
  if (identical(matches, -1L)) {
    return(0L)
  }
  length(matches)
}

test_that("outcome prior helper uses Y-only data calibration by default", {
  Y <- c(1, 2, 3, 4, 5)
  prior <- lpmec:::.lpmec_resolve_outcome_prior(Y)

  expect_equal(prior$calibration, "data")
  expect_equal(prior$intercept_mean, mean(Y))
  expect_equal(prior$intercept_sd, 2.5 * stats::sd(Y))
  expect_equal(prior$slope_mean, 0)
  expect_equal(prior$slope_sd, stats::sd(Y))
  expect_equal(prior$sigma_sd, stats::sd(Y))
})

test_that("outcome prior helper preserves legacy unit-scale priors", {
  prior <- lpmec:::.lpmec_resolve_outcome_prior(
    10 + rnorm(20),
    list(calibration = "legacy")
  )

  expect_equal(prior$calibration, "legacy")
  expect_equal(prior$intercept_mean, 0)
  expect_equal(prior$intercept_sd, 1)
  expect_equal(prior$slope_mean, 0)
  expect_equal(prior$slope_sd, 1)
  expect_equal(prior$sigma_sd, 1)
})

test_that("outcome prior helper accepts explicit numeric overrides", {
  prior <- lpmec:::.lpmec_resolve_outcome_prior(
    1:10,
    list(
      calibration = "data",
      intercept_mean = 10,
      intercept_sd = 9,
      slope_mean = -1,
      slope_sd = 8,
      sigma_sd = 7
    )
  )

  expect_equal(prior$intercept_mean, 10)
  expect_equal(prior$intercept_sd, 9)
  expect_equal(prior$slope_mean, -1)
  expect_equal(prior$slope_sd, 8)
  expect_equal(prior$sigma_sd, 7)
})

test_that("outcome prior helper validates calibration and scales", {
  expect_error(
    lpmec:::.lpmec_resolve_outcome_prior(1:10, "bad"),
    "must be a list"
  )
  expect_error(
    lpmec:::.lpmec_resolve_outcome_prior(1:10, list(calibration = "bogus")),
    "calibration"
  )
  expect_error(
    lpmec:::.lpmec_resolve_outcome_prior(1:10, list(sigma_sd = 0)),
    "sigma_sd"
  )
  expect_error(
    lpmec:::.lpmec_resolve_outcome_prior(1:10, list(extra = 1)),
    "Unknown"
  )
})

test_that("outcome prior helper floors zero outcome scale", {
  prior <- lpmec:::.lpmec_resolve_outcome_prior(rep(5, 10))

  expect_equal(prior$intercept_mean, 5)
  expect_equal(prior$intercept_sd, 2.5e-6)
  expect_equal(prior$slope_sd, 1e-6)
  expect_equal(prior$sigma_sd, 1e-6)
})

test_that("NumPyro joint model outcome priors use resolved calibration values", {
  source_path <- test_path("../../R/lpme_DoOneRun.R")
  source_text <- paste(readLines(source_path, warn = FALSE), collapse = "\n")

  expect_equal(count_matches(
    "(?s)dist\\$Normal\\(outcome_prior\\$intercept_mean,\\s*outcome_prior\\$intercept_sd\\)",
    source_text
  ), 2L)
  expect_equal(count_matches(
    "(?s)dist\\$Normal\\(outcome_prior\\$slope_mean,\\s*outcome_prior\\$slope_sd\\)",
    source_text
  ), 2L)
  expect_equal(count_matches(
    "dist\\$HalfNormal\\(outcome_prior\\$sigma_sd\\)",
    source_text
  ), 2L)

  expect_false(grepl(
    "(?s)YModel_intercept\",\\s*lpmec_env\\$dist\\$Normal\\(0, 1\\)",
    source_text,
    perl = TRUE
  ))
  expect_false(grepl(
    "(?s)YModel_slope\",\\s*lpmec_env\\$dist\\$Normal\\(0, 1\\)",
    source_text,
    perl = TRUE
  ))
  expect_false(grepl(
    "(?s)YModel_sigma\",\\s*lpmec_env\\$dist\\$HalfNormal\\(1\\)",
    source_text,
    perl = TRUE
  ))
})
