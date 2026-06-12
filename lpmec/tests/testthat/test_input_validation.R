# Tests for input validation

# Skip all tests on CRAN to avoid timeouts
skip_on_cran()

# ==============================================================================
# Y VALIDATION TESTS
# ==============================================================================

test_that("lpmec_onerun errors on NULL Y", {
  dat <- make_lpmec_test_data()
  expect_error(lpmec_onerun(NULL, dat$obs), "'Y' is required")
})

test_that("lpmec_onerun errors on non-numeric Y", {
  dat <- make_lpmec_test_data()
  Y_char <- rep("a", 80)
  expect_error(lpmec_onerun(Y_char, dat$obs), "'Y' must be a numeric vector")
})

test_that("lpmec_onerun errors on Y with too few observations", {
  dat <- make_lpmec_test_data(n = 5L)
  expect_error(lpmec_onerun(dat$Y, dat$obs), "at least 10 observations")
})

test_that("lpmec_onerun errors on all-NA Y", {
  dat <- make_lpmec_test_data()
  Y <- rep(NA_real_, 80)
  expect_error(lpmec_onerun(Y, dat$obs), "cannot be all NA")
})

# ==============================================================================
# OBSERVABLES VALIDATION TESTS
# ==============================================================================

test_that("lpmec_onerun errors on NULL observables", {
  dat <- make_lpmec_test_data()
  expect_error(lpmec_onerun(dat$Y, NULL), "'observables' is required")
})

test_that("lpmec_onerun errors on observables with wrong number of rows", {
  dat <- make_lpmec_test_data()
  short_obs <- make_lpmec_test_data(n = 50L)$obs
  expect_error(lpmec_onerun(dat$Y, short_obs), "must match length of 'Y'")
})

test_that("lpmec_onerun errors on observables with too few columns", {
  dat <- make_lpmec_test_data(p = 3L)
  expect_error(lpmec_onerun(dat$Y, dat$obs), "at least 4 columns")
})

test_that("lpmec_onerun errors on observables_groupings with wrong length", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec_onerun(dat$Y, dat$obs, observables_groupings = paste0("g", 1:4),
                 estimation_method = "pca"),
    "'observables_groupings' must have length equal to ncol\\(observables\\)"
  )
})

test_that("lpmec_onerun warns on excessive missing data", {
  dat <- make_lpmec_test_data(p = 8L, seed = 456L)
  obs <- dat$obs

  # Set > 50% of values to NA in a controlled way
  # Set columns 5-8 completely to NA (50% of columns), plus some extra
  obs[, 5:8] <- NA
  obs[1:10, 1] <- NA  # Add extra NA to push over 50%

  # The warning should be raised before any computation starts
  # Use tryCatch to verify warning is raised even if function errors later
  warning_raised <- FALSE
  tryCatch(
    withCallingHandlers(
	      lpmec_onerun(dat$Y, obs, estimation_method = "averaging"),
      warning = function(w) {
        if (grepl("More than 50%", conditionMessage(w))) {
          warning_raised <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL  # Ignore errors, we only care about the warning
  )
  expect_true(warning_raised)
})

# ==============================================================================
# ESTIMATION METHOD VALIDATION TESTS
# ==============================================================================

test_that("lpmec_onerun errors on invalid estimation_method", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec_onerun(dat$Y, dat$obs, estimation_method = "invalid_method"),
    "'estimation_method' must be one of"
  )
})

test_that("lpmec_onerun errors when custom method lacks function", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec_onerun(dat$Y, dat$obs, estimation_method = "custom"),
    "'latent_estimation_fn' is required"
  )
})

test_that("lpmec_onerun errors when latent_estimation_fn is not a function", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec_onerun(dat$Y, dat$obs, estimation_method = "custom", latent_estimation_fn = "not_a_function"),
    "'latent_estimation_fn' must be a function"
  )
})

# ==============================================================================
# ORDINAL PARAMETER VALIDATION TESTS
# ==============================================================================

test_that("lpmec_onerun errors on invalid ordinal parameter", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec_onerun(dat$Y, dat$obs, estimation_method = "pca", ordinal = "TRUE"),
    "'ordinal' must be a single logical"
  )
})

# ==============================================================================
# MCMC_CONTROL VALIDATION TESTS
# ==============================================================================

test_that("lpmec_onerun errors on invalid mcmc_control backend", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec_onerun(dat$Y, dat$obs, estimation_method = "pca",
                mcmc_control = list(backend = "invalid_backend")),
    "mcmc_control\\$backend must be either"
  )
})

test_that("lpmec_onerun errors on invalid mcmc_control n_samples_warmup", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec_onerun(dat$Y, dat$obs, estimation_method = "pca",
                mcmc_control = list(n_samples_warmup = -1)),
    "n_samples_warmup must be a positive integer"
  )
})

test_that("lpmec_onerun errors on invalid mcmc_control subsample_method", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec_onerun(dat$Y, dat$obs, estimation_method = "pca",
                 mcmc_control = list(subsample_method = "invalid")),
    "subsample_method must be either"
  )
})

test_that("lpmec_onerun errors on invalid batch_size for batch mode", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec_onerun(dat$Y, dat$obs, estimation_method = "pca",
                 mcmc_control = list(subsample_method = "batch", batch_size = 80)),
    "batch_size must be a single numeric value"
  )
})

# ==============================================================================
# LPME-SPECIFIC VALIDATION TESTS
# ==============================================================================

test_that("lpmec errors on invalid n_boot", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec(dat$Y, dat$obs, n_boot = -1, n_partition = 1, estimation_method = "pca"),
    "'n_boot' must be a single non-negative integer"
  )
  expect_error(
    lpmec(dat$Y, dat$obs, n_boot = 0.5, n_partition = 1, estimation_method = "pca"),
    "'n_boot' must be a single non-negative integer"
  )
})

test_that("lpmec errors on invalid n_partition", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec(dat$Y, dat$obs, n_boot = 1, n_partition = 0, estimation_method = "pca"),
    "'n_partition' must be a single positive integer"
  )
  expect_error(
    lpmec(dat$Y, dat$obs, n_boot = 1, n_partition = 1.5, estimation_method = "pca"),
    "'n_partition' must be a single positive integer"
  )
})

test_that("lpmec errors on boot_basis with wrong length", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec(dat$Y, dat$obs, n_boot = 1, n_partition = 1, boot_basis = 1:50,
         estimation_method = "pca"),
    "'boot_basis' must have the same length"
  )
})

test_that("lpmec errors on observables_groupings with wrong length", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec(dat$Y, dat$obs, observables_groupings = paste0("g", 1:4),
          n_boot = 1, n_partition = 1, estimation_method = "pca"),
    "'observables_groupings' must have length equal to ncol\\(observables\\)"
  )
})

test_that("lpmec errors on invalid return_intermediaries", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec(dat$Y, dat$obs, n_boot = 1, n_partition = 1, return_intermediaries = "yes",
         estimation_method = "pca"),
    "'return_intermediaries' must be a single logical"
  )
})

test_that("lpmec errors on invalid partition_aggregation", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec(dat$Y, dat$obs, n_boot = 1, n_partition = 1, estimation_method = "pca",
          partition_aggregation = "mean"),
    "'partition_aggregation' must be one of"
  )
})

test_that("lpmec errors on invalid partition_aggregation_probs", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec(dat$Y, dat$obs, n_boot = 1, n_partition = 1, estimation_method = "pca",
          partition_aggregation = "winsorized_mean",
          partition_aggregation_probs = c(0.99, 0.01)),
    "'partition_aggregation_probs' must be a numeric vector of length 2"
  )
})

test_that("lpmec errors when custom partition_aggregation is not scalar numeric", {
  dat <- make_lpmec_test_data()
  expect_error(
    lpmec(dat$Y, dat$obs, n_boot = 1, n_partition = 2, estimation_method = "pca",
          partition_aggregation = function(x) c(mean(x), stats::median(x))),
    "'partition_aggregation' must return a single numeric value"
  )
})
