# Tests for different estimation methods in lpmec

# Skip all tests on CRAN to avoid timeouts
skip_on_cran()

# ==============================================================================
# EM ESTIMATION METHOD TESTS
# ==============================================================================

test_that("lpmec_onerun works with EM estimation (default)", {
  dat <- make_lpmec_test_data()

  res <- lpmec_onerun(dat$Y, dat$obs, estimation_method = "em")

  expect_s3_class(res, "lpmec_onerun")
  expect_true(all(c("ols_coef", "x_est1", "x_est2") %in% names(res)))
  expect_equal(length(res$x_est1), 80)
  expect_equal(length(res$x_est2), 80)
  expect_true(is.numeric(res$ols_coef))
  expect_true(is.numeric(res$iv_coef))
  expect_true(is.numeric(res$corrected_ols_coef))
})

test_that("lpmec works with EM estimation (default)", {
  dat <- make_lpmec_test_data()

  res <- lpmec(dat$Y, dat$obs, n_boot = 1, n_partition = 1, estimation_method = "em")

  expect_s3_class(res, "lpmec")
  expect_true("ols_coef" %in% names(res))
  expect_true("corrected_iv_coef" %in% names(res))
  expect_true(is.numeric(res$ols_coef))
})

# ==============================================================================
# AVERAGING ESTIMATION METHOD TESTS
# ==============================================================================

test_that("lpmec_onerun works with averaging estimation", {
  dat <- make_lpmec_test_data()

  res <- lpmec_onerun(dat$Y, dat$obs, estimation_method = "averaging")

  expect_s3_class(res, "lpmec_onerun")
  expect_true(all(c("ols_coef", "x_est1", "x_est2") %in% names(res)))
  expect_equal(length(res$x_est1), 80)
  expect_true(is.numeric(res$ols_coef))
})

test_that("lpmec works with averaging estimation", {
  dat <- make_lpmec_test_data()

  res <- lpmec(dat$Y, dat$obs, n_boot = 1, n_partition = 1, estimation_method = "averaging")

  expect_s3_class(res, "lpmec")
  expect_true("ols_coef" %in% names(res))
})

# ==============================================================================
# CUSTOM ESTIMATION METHOD TESTS
# ==============================================================================

test_that("lpmec_onerun works with custom estimation function", {
  dat <- make_lpmec_test_data()

  # Simple custom function: row means
  custom_fn <- function(x) {
    rowMeans(x, na.rm = TRUE)
  }

  res <- lpmec_onerun(dat$Y, dat$obs, estimation_method = "custom",
                     latent_estimation_fn = custom_fn)

  expect_s3_class(res, "lpmec_onerun")
  expect_true(all(c("ols_coef", "x_est1", "x_est2") %in% names(res)))
  expect_equal(length(res$x_est1), 80)
})

test_that("lpmec works with custom estimation function", {
  dat <- make_lpmec_test_data()

  custom_fn <- function(x) {
    rowMeans(x, na.rm = TRUE)
  }

  res <- lpmec(dat$Y, dat$obs, n_boot = 1, n_partition = 1,
              estimation_method = "custom",
              latent_estimation_fn = custom_fn)

  expect_s3_class(res, "lpmec")
  expect_true("ols_coef" %in% names(res))
})

test_that("custom estimation with PCA-like function works", {
  dat <- make_lpmec_test_data()

  # Custom PCA-like function
  custom_pca <- function(x) {
    x_mat <- as.matrix(x)
    x_mat[is.na(x_mat)] <- 0
    pca_out <- prcomp(x_mat, center = TRUE, scale. = TRUE)
    pca_out$x[, 1]
  }

  res <- lpmec_onerun(dat$Y, dat$obs, estimation_method = "custom",
                     latent_estimation_fn = custom_pca)

  expect_s3_class(res, "lpmec_onerun")
  expect_true(is.numeric(res$ols_coef))
})
