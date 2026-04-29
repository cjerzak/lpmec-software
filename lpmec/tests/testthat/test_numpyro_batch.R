# Tests for NumPyro batch-mode guardrails

skip_on_cran()

test_that("NumPyro batch model relies on subsampled plate scaling", {
  source_path <- test_path("../../R/lpme_DoOneRun.R")
  source_text <- paste(readLines(source_path, warn = FALSE), collapse = "\n")

  expect_false(grepl("handlers\\$scale", source_text, fixed = FALSE))
  expect_false(grepl("scaled_lik", source_text, fixed = TRUE))
  expect_true(grepl("subsampled plate by", source_text, fixed = TRUE))
})

test_that("NumPyro subsampled plates scale observed likelihood sites", {
  skip_if_not(reticulate::py_module_available("jax"))
  skip_if_not(reticulate::py_module_available("numpyro"))

  `%as%` <- get("%as%", asNamespace("reticulate"))
  jnp <- reticulate::import("jax.numpy")
  numpyro <- reticulate::import("numpyro")
  dist <- reticulate::import("numpyro.distributions")

  N <- 8L
  batch_size <- 2L
  expected_scale <- N / batch_size

  model <- function(X, Y) {
    Y_intercept <- numpyro$sample("YModel_intercept", dist$Normal(0, 1))
    with(numpyro$plate(
      "rows_subsample",
      size = N,
      subsample_size = batch_size,
      dim = -1L
    ) %as% "idx", {
      X_sub <- jnp$take(X, idx, axis = 0L)
      Y_sub <- jnp$take(Y, idx, axis = 0L)
      numpyro$sample("Xlik_sub", dist$Bernoulli(probs = 0.5), obs = X_sub)
      numpyro$sample("Ylik_sub", dist$Normal(Y_intercept, 1), obs = Y_sub)
    })
  }

  trace <- numpyro$handlers$trace(
    numpyro$handlers$seed(model, rng_seed = 0L)
  )$get_trace(jnp$array(rep(0, N)), jnp$array(rnorm(N)))

  expect_equal(as.numeric(trace$Xlik_sub$scale), expected_scale)
  expect_equal(as.numeric(trace$Ylik_sub$scale), expected_scale)
  if (!is.null(trace$YModel_intercept$scale)) {
    expect_equal(as.numeric(trace$YModel_intercept$scale), 1)
  }
})
