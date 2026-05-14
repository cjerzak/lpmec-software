# Tests for NumPyro mixed factor benchmark mcmc_joint2

skip_on_cran()

make_joint2_data <- function(n = 16L) {
  theta <- seq(-1.5, 1.5, length.out = n)
  obs <- cbind(
    as.integer(theta > -0.8),
    as.integer(theta > -0.2),
    as.integer(theta > 0.2),
    as.integer(theta > 0.8)
  )
  obs <- as.data.frame(obs)
  names(obs) <- paste0("k", seq_len(ncol(obs)))
  Y <- 0.5 + 0.7 * theta + seq(-0.15, 0.15, length.out = n)
  list(Y = Y, obs = obs)
}

test_that("mcmc_joint2 validation is method-specific", {
  dat <- make_joint2_data()

  expect_error(
    lpmec_onerun(dat$Y, dat$obs, estimation_method = "mcmc_joint2"),
    "requires mcmc_control\\$backend = 'numpyro'"
  )
  expect_error(
    lpmec(dat$Y, dat$obs, n_boot = 0, n_partition = 1,
          estimation_method = "mcmc_joint2"),
    "requires mcmc_control\\$backend = 'numpyro'"
  )

  dat$obs[1, 1] <- 2
  expect_error(
    lpmec_onerun(
      dat$Y,
      dat$obs,
      estimation_method = "mcmc_joint2",
      mcmc_control = list(backend = "numpyro")
    ),
    "binary 0/1"
  )
})

test_that("mcmc_joint2 rejects ordinal observables", {
  dat <- make_joint2_data()

  expect_error(
    lpmec_onerun(
      dat$Y,
      dat$obs,
      estimation_method = "mcmc_joint2",
      ordinal = TRUE,
      mcmc_control = list(backend = "numpyro")
    ),
    "ordinal = TRUE is not supported",
    fixed = TRUE
  )
})

test_that("mcmc_joint2 prior helper uses MCMCpack-style defaults and validates overrides", {
  prior <- lpmec:::.lpmec_resolve_joint2_prior()

  expect_equal(prior$lambda_mean, 0)
  expect_equal(prior$lambda_sd, 2)
  expect_equal(prior$psi_shape, 0.0005)
  expect_equal(prior$psi_scale, 0.0005)

  override <- lpmec:::.lpmec_resolve_joint2_prior(list(
    lambda_mean = 1,
    lambda_sd = 3,
    psi_shape = 2,
    psi_scale = 4
  ))
  expect_equal(override$lambda_mean, 1)
  expect_equal(override$lambda_sd, 3)
  expect_equal(override$psi_shape, 2)
  expect_equal(override$psi_scale, 4)

  expect_error(
    lpmec:::.lpmec_resolve_joint2_prior(list(lambda_sd = 0)),
    "lambda_sd"
  )
  expect_error(
    lpmec:::.lpmec_resolve_joint2_prior(list(extra = 1)),
    "Unknown"
  )
})

test_that("mcmc_joint2 slope helper matches the inner-normalized formula", {
  theta_draws <- matrix(
    c(
      -1, 0, 1,
      -2, 0, 2,
      0, 1, 2
    ),
    nrow = 3
  )
  lambda_y1_draws <- c(0.5, 1.25, -0.75)

  slopes <- lpmec:::.lpmec_mcmc_joint2_bayesian_slopes(
    lambda_y1_draws,
    theta_draws
  )

  expect_equal(
    slopes$bayesian_ols_coef_inner_normed,
    mean(lambda_y1_draws * apply(theta_draws, 2, stats::sd))
  )
  expect_equal(
    slopes$bayesian_ols_se_inner_normed,
    stats::sd(lambda_y1_draws * apply(theta_draws, 2, stats::sd))
  )
})

test_that("mcmc_joint2 orientation helper aligns reflected draws and slopes", {
  obs <- matrix(
    c(
      0, 0, 0,
      0, 0, 1,
      0, 1, 1,
      1, 1, 1
    ),
    nrow = 4,
    byrow = TRUE
  )
  theta_reference <- as.numeric(scale(rowMeans(obs)))
  theta_draws <- cbind(theta_reference, -theta_reference)
  lambda_y1_draws <- c(0.7, -0.7)

  oriented <- lpmec:::.lpmec_mcmc_joint2_orient_draws(
    theta_draws,
    lambda_y1_draws,
    obs
  )
  slopes <- lpmec:::.lpmec_mcmc_joint2_bayesian_slopes(
    oriented$lambda_y1_draws,
    oriented$theta_draws
  )

  expect_equal(oriented$signs, c(1, -1))
  expect_equal(oriented$lambda_y1_draws, c(0.7, 0.7))
  expect_true(all(slopes$inner_draws > 0))
  expect_equal(unname(slopes$inner_draws), rep(0.7 * stats::sd(theta_reference), 2))
  expect_equal(slopes$bayesian_ols_coef_inner_normed, 0.7)
  expect_equal(oriented$n_flipped, 1)
  expect_equal(oriented$prop_flipped, 0.5)
  expect_equal(oriented$min_abs_draw_reference_cor, 1)
})

test_that("mcmc_joint2 orientation helper falls back to anchor when correlation is unavailable", {
  obs <- matrix(0, nrow = 3, ncol = 4)
  theta_draws <- cbind(c(1, -1, 0), c(-1, 1, 0))
  lambda_y1_draws <- c(0.4, -0.4)

  oriented <- lpmec:::.lpmec_mcmc_joint2_orient_draws(
    theta_draws,
    lambda_y1_draws,
    obs
  )

  expect_equal(oriented$signs, c(1, -1))
  expect_equal(oriented$lambda_y1_draws, c(0.4, 0.4))
  expect_true(is.na(oriented$min_abs_draw_reference_cor))
})

test_that("mcmc_joint2 does not call MCMCpack and preserves the mcmc_joint branch", {
  source_text <- paste(
    readLines(test_path("../../R/lpme_DoOneRun.R"), warn = FALSE),
    readLines(test_path("../../R/lpme_HelperFxns.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("MCMCmixfactanal", source_text, fixed = TRUE))
  expect_true(grepl('estimation_method == "mcmc_joint"', source_text, fixed = TRUE))
  expect_true(grepl('estimation_method == "mcmc_joint2"', source_text, fixed = TRUE))
})

test_that("mcmc_joint2 NumPyro smoke test returns finite estimates and diagnostics", {
  reticulate::use_condaenv("lpmec", required = FALSE)
  skip_if_not(reticulate::py_module_available("jax"))
  skip_if_not(reticulate::py_module_available("numpyro"))

  dat <- make_joint2_data(n = 12L)
  res <- lpmec_onerun(
    dat$Y,
    dat$obs,
    estimation_method = "mcmc_joint2",
    mcmc_control = list(
      backend = "numpyro",
      n_samples_warmup = 6L,
      n_samples_mcmc = 6L,
      chain_method = "sequential",
      n_thin_by = 1L,
      n_chains = 2L,
      progress_bar = FALSE,
      seed = 19L,
      joint2_prior = list(psi_shape = 2, psi_scale = 1)
    )
  )

  expect_s3_class(res, "lpmec_onerun")
  expect_true(is.finite(res$bayesian_ols_coef_inner_normed))
  expect_true(is.finite(res$bayesian_ols_se_inner_normed))
  expect_true(is.finite(res$bayesian_ols_coef_outer_normed))
  expect_true(is.finite(res$bayesian_ols_se_outer_normed))
  expect_true(all(is.finite(res$x_est1)))
  expect_true(all(is.finite(res$x_est2)))
  expect_true(is.finite(res$mcmc_joint2_ability_mean_ess_pct))
  expect_true(is.finite(res$mcmc_joint2_ability_min_ess_pct))
  expect_true(is.finite(res$mcmc_joint2_max_rhat))
  expect_true(is.finite(res$mcmc_joint2_num_divergent))
  expect_true(is.finite(res$mcmc_joint2_mean_accept_prob))
  expect_true(is.finite(res$mcmc_joint2_orientation_n_flipped))
  expect_true(is.finite(res$mcmc_joint2_orientation_prop_flipped))
  expect_true(is.finite(res$mcmc_joint2_orientation_min_abs_cor))
})
