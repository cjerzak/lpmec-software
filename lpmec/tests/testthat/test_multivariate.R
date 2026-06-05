# Skip all tests on CRAN to avoid timeouts
skip_on_cran()

make_multivariate_fixture <- function(n = 120L, k = 2L, m = 6L) {
  x <- matrix(rnorm(n * k), nrow = n, ncol = k)
  colnames(x) <- paste0("theta", seq_len(k))
  beta <- seq(0.3, 0.5, length.out = k)
  y <- as.numeric(0.2 + x %*% beta + rnorm(n, sd = 0.4))
  observables <- lapply(seq_len(k), function(j) {
    obs <- x[, j] + matrix(rnorm(n * m, sd = 0.25), nrow = n, ncol = m)
    colnames(obs) <- paste0("item", seq_len(m))
    obs
  })
  names(observables) <- paste0("latent", seq_len(k))
  list(Y = y, observables = observables)
}

test_that("lpmec_multivariate_onerun returns multivariate corrections", {
  set.seed(123)
  dat <- make_multivariate_fixture()
  res <- lpmec_multivariate_onerun(
    Y = dat$Y,
    observables = dat$observables,
    estimation_method = "averaging",
    min_split_correlation = 0.05
  )

  expect_s3_class(res, "lpmec_multivariate_onerun")
  expect_length(res$ols_coef, 2)
  expect_length(res$corrected_iv_coef, 2)
  expect_true(all(is.finite(res$corrected_iv_coef)))
  expect_true(all(res$split_correlation > 0.05))
  expect_equal(dim(res$x_est1), c(length(dat$Y), 2L))
})

test_that("lpmec_multivariate aggregates partitions and bootstraps", {
  set.seed(456)
  dat <- make_multivariate_fixture(n = 100L)
  res <- lpmec_multivariate(
    Y = dat$Y,
    observables = dat$observables,
    estimation_method = "averaging",
    n_boot = 1,
    n_partition = 1,
    min_split_correlation = 0.05
  )

  expect_s3_class(res, "lpmec_multivariate")
  expect_length(res$ols_coef, 2)
  expect_length(res$corrected_iv_coef, 2)
  expect_true("Intermediary_corrected_iv_coef" %in% names(res))
  expect_equal(dim(res$Intermediary_corrected_iv_coef), c(2L, 2L))
})
