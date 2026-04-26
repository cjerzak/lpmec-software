# Tests for bootstrap aggregation functionality

# Skip all tests on CRAN to avoid timeouts
skip_on_cran()

# ==============================================================================
# BOOTSTRAP AGGREGATION TESTS
# ==============================================================================

test_that("partition aggregation helpers implement built-in strategies", {
  median_fxn <- lpmec:::.lpmec_resolve_partition_aggregation("median", c(0.01, 0.99))
  expect_equal(median_fxn(c(1, 2, 100)), stats::median(c(1, 2, 100)))

  x <- c(1, 2, 3, 100)
  winsor_fxn <- lpmec:::.lpmec_resolve_partition_aggregation(
    "winsorized_mean",
    c(0.25, 0.75)
  )
  winsor_bounds <- stats::quantile(x, probs = c(0.25, 0.75), names = FALSE)
  expect_equal(
    winsor_fxn(x),
    mean(pmin(pmax(x, winsor_bounds[1]), winsor_bounds[2]))
  )
  expect_true(is.na(winsor_fxn(c(1, NA_real_, 100))))

  trimmed_fxn <- lpmec:::.lpmec_resolve_partition_aggregation(
    "trimmed_mean",
    c(0.25, 0.75)
  )
  expect_equal(trimmed_fxn(x), 2.5)

  custom_fxn <- lpmec:::.lpmec_resolve_partition_aggregation(
    function(values) max(values) - min(values),
    c(0.01, 0.99)
  )
  expect_equal(custom_fxn(c(1, 4, 10)), 9)
})

test_that("lpmec bootstrap aggregation works with n_boot > 1", {
  set.seed(123)
  Y <- rnorm(80)
  obs <- as.data.frame(matrix(sample(c(0, 1), 80 * 6, replace = TRUE), ncol = 6))

  res <- lpmec(Y, obs, n_boot = 3, n_partition = 2, estimation_method = "pca")

  expect_s3_class(res, "lpmec")
  # Bootstrap should produce standard errors
  expect_true("ols_se" %in% names(res))
  expect_true("corrected_iv_se" %in% names(res))
  # Standard errors should be positive (non-NA) when n_boot > 1
  expect_true(is.numeric(res$ols_se))
  expect_true(is.numeric(res$corrected_iv_se))
})

test_that("lpmec partition aggregation works with n_partition > 1", {
  set.seed(123)
  Y <- rnorm(80)
  obs <- as.data.frame(matrix(sample(c(0, 1), 80 * 6, replace = TRUE), ncol = 6))

  res <- lpmec(Y, obs, n_boot = 1, n_partition = 3, estimation_method = "pca")

  expect_s3_class(res, "lpmec")
  expect_true("ols_coef" %in% names(res))
  expect_true(is.numeric(res$ols_coef))
})

test_that("lpmec produces confidence intervals with sufficient bootstrap", {
  set.seed(123)
  Y <- rnorm(80)
  obs <- as.data.frame(matrix(sample(c(0, 1), 80 * 6, replace = TRUE), ncol = 6))

  res <- lpmec(Y, obs, n_boot = 5, n_partition = 2, estimation_method = "pca")

  # Should have lower and upper bounds

  expect_true("ols_lower" %in% names(res))
  expect_true("ols_upper" %in% names(res))
  expect_true("corrected_iv_lower" %in% names(res))
  expect_true("corrected_iv_upper" %in% names(res))

  # Lower should be less than upper
  if (!is.na(res$ols_lower) && !is.na(res$ols_upper)) {
    expect_true(res$ols_lower <= res$ols_upper)
  }
})

test_that("lpmec with stratified bootstrap (boot_basis) works", {
  set.seed(123)
  Y <- rnorm(80)
  obs <- as.data.frame(matrix(sample(c(0, 1), 80 * 6, replace = TRUE), ncol = 6))
  # Create a grouping variable for stratified bootstrap
  boot_groups <- rep(1:10, each = 8)

  res <- lpmec(Y, obs, n_boot = 2, n_partition = 1, boot_basis = boot_groups,
              estimation_method = "pca")

  expect_s3_class(res, "lpmec")
  expect_true("ols_coef" %in% names(res))
})

test_that("lpmec intermediary results are stored when return_intermediaries = TRUE", {
  set.seed(123)
  Y <- rnorm(80)
  obs <- as.data.frame(matrix(sample(c(0, 1), 80 * 6, replace = TRUE), ncol = 6))

  res <- lpmec(Y, obs, n_boot = 2, n_partition = 2, estimation_method = "pca",
              return_intermediaries = TRUE)

  expect_s3_class(res, "lpmec")
  # Should have x_est1 and x_est2 from first run
  expect_true("x_est1" %in% names(res))
  expect_true("x_est2" %in% names(res))
  expect_equal(length(res$x_est1), 80)
})

test_that("lpmec var_est_split is computed correctly", {
  set.seed(123)
  Y <- rnorm(80)
  obs <- as.data.frame(matrix(sample(c(0, 1), 80 * 6, replace = TRUE), ncol = 6))

  res <- lpmec(Y, obs, n_boot = 2, n_partition = 3, estimation_method = "pca")

  # var_est_split should exist
  expect_true("var_est_split" %in% names(res))
  # It should be numeric (could be NA in edge cases)
  expect_true(is.numeric(res$var_est_split) || is.na(res$var_est_split))
})

test_that("lpmec default partition aggregation matches explicit median", {
  set.seed(123)
  Y <- rnorm(80)
  obs <- as.data.frame(matrix(sample(c(0, 1), 80 * 6, replace = TRUE), ncol = 6))

  set.seed(456)
  default_res <- lpmec(Y, obs, n_boot = 1, n_partition = 3,
                       estimation_method = "pca")
  set.seed(456)
  median_res <- lpmec(Y, obs, n_boot = 1, n_partition = 3,
                      estimation_method = "pca",
                      partition_aggregation = "median")

  expect_equal(default_res$ols_coef, median_res$ols_coef)
  expect_equal(default_res$corrected_iv_coef, median_res$corrected_iv_coef)
  expect_equal(default_res$var_est_split, median_res$var_est_split)
})

test_that("lpmec supports winsorized and custom partition aggregation", {
  set.seed(123)
  Y <- rnorm(80)
  obs <- as.data.frame(matrix(sample(c(0, 1), 80 * 6, replace = TRUE), ncol = 6))

  winsorized_res <- lpmec(Y, obs, n_boot = 1, n_partition = 3,
                          estimation_method = "pca",
                          partition_aggregation = "winsorized_mean")
  custom_res <- lpmec(Y, obs, n_boot = 1, n_partition = 3,
                      estimation_method = "pca",
                      partition_aggregation = function(x) mean(x))

  expect_s3_class(winsorized_res, "lpmec")
  expect_s3_class(custom_res, "lpmec")
  expect_true(is.numeric(winsorized_res$ols_coef))
  expect_true(is.numeric(custom_res$ols_coef))
})
