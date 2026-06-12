# Tests for m-out-of-n/subsampling bootstrap helpers and fixed partitions

skip_on_cran()

test_that("m resolution handles n-out-of-n, power m, and explicit invalid m", {
  expect_equal(
    lpmec:::.lpmec_resolve_m(100, bootstrap_method = "n_out_of_n")$m,
    100L
  )

  m_spec <- lpmec:::.lpmec_resolve_m(
    100,
    bootstrap_method = "subsampling",
    boot_m_exponent = 0.70,
    min_m = 10L
  )
  expect_equal(m_spec$m, floor(100^0.70))
  expect_equal(m_spec$m_rule, "power")

  expect_error(
    lpmec:::.lpmec_resolve_m(
      50,
      boot_m = 50,
      bootstrap_method = "m_out_of_n"
    ),
    "2 <= boot_m < n"
  )
})

test_that("median aggregation resolves n-out-of-n bootstrap without advisory warning", {
  expect_silent({
    boot_spec <- lpmec:::.lpmec_resolve_bootstrap_method(
      bootstrap_method = "n_out_of_n",
      partition_aggregation = "median",
      boot_ci_type = "auto"
    )
  })
  expect_equal(boot_spec$bootstrap_method, "n_out_of_n")
  expect_equal(boot_spec$boot_ci_type, "percentile")
})

test_that("resampling indices have requested length and respect replacement", {
  set.seed(1)
  idx <- lpmec:::.lpmec_resample_indices(30, 12, replace = FALSE)
  expect_length(idx, 12)
  expect_equal(length(unique(idx)), 12)

  set.seed(2)
  idx_replace <- lpmec:::.lpmec_resample_indices(5, 20, replace = TRUE)
  expect_length(idx_replace, 20)
  expect_true(anyDuplicated(idx_replace) > 0L)

  set.seed(3)
  strata <- rep(c("a", "b", "c"), times = c(5, 10, 15))
  idx_strat <- lpmec:::.lpmec_resample_indices(
    30,
    14,
    replace = FALSE,
    boot_basis = strata
  )
  expect_length(idx_strat, 14)
  expect_equal(length(unique(idx_strat)), 14)
})

test_that("partition construction is balanced, unique, and validates supplied sets", {
  groupings <- c("A", "A", "B", "C", "D", "E")
  parts <- lpmec:::.lpmec_make_partitions(groupings, n_partition = 4, seed = 1)
  groups <- unique(groupings)

  expect_length(parts, 4)
  keys <- vapply(parts, function(partition) {
    expect_equal(sort(c(partition$split1_names, partition$split2_names)), sort(groups))
    expect_lte(abs(length(partition$split1_names) - length(partition$split2_names)), 1)
    lpmec:::.lpmec_partition_key(partition$split1_names, partition$split2_names)
  }, character(1))
  expect_equal(length(unique(keys)), length(keys))

  supplied <- list(
    list(partition_id = "p1", split1_names = c("A", "B"), split2_names = c("C", "D", "E"))
  )
  respected <- lpmec:::.lpmec_make_partitions(
    groupings,
    n_partition = 1,
    partition_set = supplied
  )
  expect_equal(respected[[1]]$partition_id, "p1")
  expect_equal(respected[[1]]$split1_names, supplied[[1]]$split1_names)
  expect_equal(respected[[1]]$split2_names, supplied[[1]]$split2_names)
})

test_that("lpmec_onerun respects supplied fixed partitions", {
  set.seed(10)
  Y <- rnorm(80)
  obs <- as.data.frame(matrix(sample(c(0, 1), 80 * 6, replace = TRUE), ncol = 6))
  partition <- list(
    partition_id = "fixed",
    split1_names = c("V1", "V2", "V3"),
    split2_names = c("V4", "V5", "V6")
  )

  fit <- lpmec_onerun(
    Y = Y,
    observables = obs,
    estimation_method = "averaging",
    partition = partition
  )

  expect_equal(fit$partition_id, "fixed")
  expect_equal(fit$split1_names, partition$split1_names)
  expect_equal(fit$split2_names, partition$split2_names)
  expect_true(is.numeric(fit$split_correlation))
})

test_that("root interval helper implements m-to-n scaling exactly", {
  theta0 <- 10
  theta_boot <- c(9, 11, 12, NA, Inf)
  n <- 100
  m <- 25
  alpha <- 0.20
  root_draws <- sqrt(m) * (theta_boot[1:3] - theta0)

  summary <- suppressWarnings(lpmec:::.lpmec_summarize_resampling(
    theta0 = theta0,
    theta_boot = theta_boot,
    n = n,
    m = m,
    bootstrap_method = "subsampling",
    boot_ci_type = "root",
    alpha = alpha
  ))

  expect_equal(summary$root_draws, root_draws)
  expect_equal(summary$se, stats::sd(root_draws) / sqrt(n))
  expect_equal(
    summary$lower,
    theta0 - stats::quantile(root_draws, 1 - alpha / 2, names = FALSE) / sqrt(n)
  )
  expect_equal(
    summary$upper,
    theta0 - stats::quantile(root_draws, alpha / 2, names = FALSE) / sqrt(n)
  )
  expect_equal(summary$n_failed, 2L)
})

test_that("root interval helper accepts asymmetric calibrated tails", {
  theta0 <- 10
  theta_boot <- c(8, 9, 11, 12, 13)
  n <- 100
  m <- 25
  root_draws <- sqrt(m) * (theta_boot - theta0)

  summary <- lpmec:::.lpmec_summarize_resampling(
    theta0 = theta0,
    theta_boot = theta_boot,
    n = n,
    m = m,
    bootstrap_method = "subsampling",
    boot_ci_type = "root_calibrated",
    alpha = 0.20,
    root_left_tail = 0.05,
    root_right_tail = 0.15
  )

  expect_equal(summary$root_left_tail, 0.05)
  expect_equal(summary$root_right_tail, 0.15)
  expect_equal(
    summary$lower,
    theta0 - stats::quantile(root_draws, 1 - 0.15, names = FALSE) / sqrt(n)
  )
  expect_equal(
    summary$upper,
    theta0 - stats::quantile(root_draws, 0.05, names = FALSE) / sqrt(n)
  )
})

test_that("root calibration cutoff selector chooses largest admissible cutoffs", {
  coverage <- data.frame(
    field = "theta",
    cut = c(0.010, 0.025, 0.050),
    lower_coverage = c(1.000, 0.980, 0.940),
    upper_coverage = c(0.990, 0.960, 0.940),
    twosided_coverage = c(0.980, 0.950, 0.900)
  )

  separate <- lpmec:::.lpmec_select_root_calibration_cutoffs(
    coverage,
    alpha = 0.05,
    tail = "separate"
  )
  expect_equal(separate$root_right_tail, 0.025)
  expect_equal(separate$root_left_tail, 0.010)

  equal_tail <- lpmec:::.lpmec_select_root_calibration_cutoffs(
    coverage,
    alpha = 0.05,
    tail = "equal_tail"
  )
  expect_equal(equal_tail$root_right_tail, 0.025)
  expect_equal(equal_tail$root_left_tail, 0.025)
})

test_that("root calibration validates method and grid inputs", {
  expect_error(
    lpmec:::.lpmec_validate_root_calibration(
      boot_ci_type = "root_calibrated",
      bootstrap_method = "n_out_of_n",
      n_boot = 10L,
      boot_calibration_n = 2L,
      boot_calibration_inner_n_boot = 2L,
      boot_calibration_cut_grid = c(0.025, 0.050),
      boot_calibration_tail = "separate",
      boot_calibration_seed = NULL,
      alpha = 0.05
    ),
    "requires bootstrap_method"
  )

  expect_error(
    lpmec:::.lpmec_validate_root_calibration(
      boot_ci_type = "root_calibrated",
      bootstrap_method = "subsampling",
      n_boot = 10L,
      boot_calibration_n = 2L,
      boot_calibration_inner_n_boot = 2L,
      boot_calibration_cut_grid = c(0, 0.025),
      boot_calibration_tail = "separate",
      boot_calibration_seed = NULL,
      alpha = 0.05
    ),
    "boot_calibration_cut_grid"
  )

  expect_silent(lpmec:::.lpmec_validate_root_calibration(
    boot_ci_type = "root_calibrated",
    bootstrap_method = "subsampling",
    n_boot = 10L,
    boot_calibration_n = 2L,
    boot_calibration_inner_n_boot = 2L,
    boot_calibration_cut_grid = c(0.025, 0.050),
    boot_calibration_tail = "separate",
    boot_calibration_seed = NULL,
    alpha = 0.05
  ))
})

test_that("lpmec keeps supplied partitions fixed across bootstrap replicates", {
  set.seed(11)
  n <- 90
  x <- rnorm(n)
  Y <- 0.5 * x + rnorm(n, sd = 0.4)
  obs <- as.data.frame(sapply(seq_len(6), function(j) {
    stats::rbinom(n, 1, stats::plogis(x + rnorm(n, sd = 0.3)))
  }))
  partition_set <- list(
    list(partition_id = "p1", split1_names = c("V1", "V2", "V3"), split2_names = c("V4", "V5", "V6")),
    list(partition_id = "p2", split1_names = c("V1", "V4", "V5"), split2_names = c("V2", "V3", "V6"))
  )

  fit <- suppressWarnings(suppressMessages(lpmec(
    Y = Y,
    observables = obs,
    n_boot = 3L,
    n_partition = 2L,
    estimation_method = "averaging",
    partition_aggregation = "median",
    bootstrap_method = "subsampling",
    boot_m = 40L,
    partition_set = partition_set,
    seed = 99
  )))

  expect_s3_class(fit, "lpmec")
  expect_equal(fit$bootstrap_method, "subsampling")
  expect_equal(fit$boot_m, 40L)
  expect_equal(fit$boot_ci_type, "root")
  expect_equal(as.character(c(fit$Intermediary_partition_id)), rep(c("p1", "p2"), times = 4))

  split1_by_run <- apply(fit$Intermediary_split1_names, 2, paste, collapse = "|")
  expect_equal(split1_by_run, rep(c("V1|V2|V3", "V1|V4|V5"), times = 4))
  expect_true(is.finite(fit$corrected_iv_coef))
  expect_true(is.finite(fit$corrected_iv_se))
  expect_true("corrected_iv_coef" %in% names(fit$root_draws))
  expect_equal(nrow(fit$bootstrap_aggregates), 4L)
})

test_that("lpmec runs nested calibrated root intervals on a small averaging fit", {
  set.seed(12)
  n <- 90
  x <- rnorm(n)
  Y <- 0.6 * x + rnorm(n, sd = 0.4)
  obs <- as.data.frame(sapply(seq_len(6), function(j) {
    stats::rbinom(n, 1, stats::plogis(1.2 * x + rnorm(n, sd = 0.2)))
  }))
  partition_set <- list(
    list(partition_id = "p1", split1_names = c("V1", "V2", "V3"), split2_names = c("V4", "V5", "V6")),
    list(partition_id = "p2", split1_names = c("V1", "V4", "V5"), split2_names = c("V2", "V3", "V6"))
  )

  fit <- suppressWarnings(suppressMessages(lpmec(
    Y = Y,
    observables = obs,
    n_boot = 2L,
    n_partition = 2L,
    estimation_method = "averaging",
    partition_aggregation = "median",
    bootstrap_method = "subsampling",
    boot_m = 40L,
    boot_ci_type = "root_calibrated",
    boot_calibration_n = 1L,
    boot_calibration_inner_n_boot = 2L,
    boot_calibration_cut_grid = c(0.025, 0.050),
    boot_calibration_seed = 101,
    partition_set = partition_set,
    seed = 99,
    return_intermediaries = FALSE
  )))

  expect_s3_class(fit, "lpmec")
  expect_equal(fit$boot_ci_type, "root_calibrated")
  expect_equal(fit$boot_calibration, "nested_subsampling")
  expect_equal(fit$boot_calibration_tail, "separate")
  expect_true(is.data.frame(fit$boot_calibration_coverage))
  expect_true(is.data.frame(fit$boot_calibration_diagnostics))
  expect_true("corrected_iv_coef" %in% fit$boot_calibration_diagnostics$field)
  expect_true("corrected_iv_coef" %in% names(fit$root_draws))
})
