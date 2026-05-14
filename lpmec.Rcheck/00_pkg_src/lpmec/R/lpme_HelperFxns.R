f2n <- function(x){as.numeric( as.character( x ) ) }
f2i <- function(f_){lpmec_env$jnp$array(f_,lpmec_env$jnp$int32)}
f2a <- function(x){lpmec_env$jnp$array(x,lpmec_env$jnp$float32)}
ai <- as.integer
lpmec_env <- new.env( parent = emptyenv() )

.lpmec_validate_partition_aggregation_probs <- function(partition_aggregation_probs) {
  if (!is.numeric(partition_aggregation_probs) ||
      length(partition_aggregation_probs) != 2L ||
      any(!is.finite(partition_aggregation_probs)) ||
      any(partition_aggregation_probs < 0 | partition_aggregation_probs > 1) ||
      partition_aggregation_probs[1] >= partition_aggregation_probs[2]) {
    stop("'partition_aggregation_probs' must be a numeric vector of length 2 ",
         "with finite ordered values in [0, 1].")
  }
}

.lpmec_scalar_numeric <- function(value, aggregation_label) {
  if (!is.numeric(value) || length(value) != 1L) {
    stop("'", aggregation_label, "' must return a single numeric value.")
  }
  as.numeric(value)
}

.lpmec_winsorized_mean <- function(x, probs) {
  if (anyNA(x)) {
    return(NA_real_)
  }
  bounds <- stats::quantile(x, probs = probs, names = FALSE)
  mean(pmin(pmax(x, bounds[1]), bounds[2]))
}

.lpmec_trimmed_mean <- function(x, probs) {
  if (anyNA(x)) {
    return(NA_real_)
  }
  bounds <- stats::quantile(x, probs = probs, names = FALSE)
  mean(x[x >= bounds[1] & x <= bounds[2]])
}

.lpmec_resolve_partition_aggregation <- function(partition_aggregation,
                                                 partition_aggregation_probs) {
  if (is.function(partition_aggregation)) {
    return(function(x) {
      .lpmec_scalar_numeric(partition_aggregation(x), "partition_aggregation")
    })
  }

  valid_aggregations <- c("median", "winsorized_mean", "trimmed_mean")
  if (!is.character(partition_aggregation) ||
      length(partition_aggregation) != 1L ||
      !partition_aggregation %in% valid_aggregations) {
    stop("'partition_aggregation' must be one of: ",
         paste(valid_aggregations, collapse = ", "),
         ", or a function.")
  }

  .lpmec_validate_partition_aggregation_probs(partition_aggregation_probs)

  switch(
    partition_aggregation,
    median = function(x) stats::median(x),
    winsorized_mean = function(x) .lpmec_winsorized_mean(x, partition_aggregation_probs),
    trimmed_mean = function(x) .lpmec_trimmed_mean(x, partition_aggregation_probs)
  )
}

.lpmec_validate_outcome_prior_scalar <- function(value, name, positive = FALSE) {
  if (!is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      (positive && value <= 0)) {
    qualifier <- if (positive) "a single positive finite numeric value" else
      "a single finite numeric value"
    stop("mcmc_control$outcome_prior$", name, " must be ", qualifier, ".")
  }
  as.numeric(value)
}

.lpmec_resolve_outcome_prior <- function(Y, outcome_prior = list()) {
  if (is.null(outcome_prior)) {
    outcome_prior <- list()
  }
  if (!is.list(outcome_prior)) {
    stop("mcmc_control$outcome_prior must be a list.")
  }

  valid_names <- c(
    "calibration",
    "scale_floor",
    "intercept_mean",
    "intercept_sd",
    "slope_mean",
    "slope_sd",
    "sigma_sd"
  )
  unknown_names <- setdiff(names(outcome_prior), valid_names)
  if (length(unknown_names) > 0L) {
    stop("Unknown mcmc_control$outcome_prior field(s): ",
         paste(unknown_names, collapse = ", "), ".")
  }

  calibration <- outcome_prior$calibration
  if (is.null(calibration)) {
    calibration <- "data"
  }
  if (!is.character(calibration) ||
      length(calibration) != 1L ||
      !calibration %in% c("data", "legacy")) {
    stop("mcmc_control$outcome_prior$calibration must be either 'data' or 'legacy'.")
  }

  scale_floor <- outcome_prior$scale_floor
  if (is.null(scale_floor)) {
    scale_floor <- 1e-6
  }
  scale_floor <- .lpmec_validate_outcome_prior_scalar(
    scale_floor,
    "scale_floor",
    positive = TRUE
  )

  if (calibration == "legacy") {
    resolved <- list(
      calibration = calibration,
      intercept_mean = 0,
      intercept_sd = 1,
      slope_mean = 0,
      slope_sd = 1,
      sigma_sd = 1
    )
  } else {
    finite_y <- Y[is.finite(Y)]
    if (length(finite_y) < 1L) {
      stop("Outcome prior calibration requires at least one finite value in 'Y'.")
    }
    y_sd <- stats::sd(finite_y)
    if (!is.finite(y_sd) || y_sd < scale_floor) {
      y_sd <- scale_floor
    }

    resolved <- list(
      calibration = calibration,
      intercept_mean = mean(finite_y),
      intercept_sd = 2.5 * y_sd,
      slope_mean = 0,
      slope_sd = y_sd,
      sigma_sd = y_sd
    )
  }

  for (name in setdiff(valid_names, c("calibration", "scale_floor"))) {
    if (!is.null(outcome_prior[[name]])) {
      resolved[[name]] <- .lpmec_validate_outcome_prior_scalar(
        outcome_prior[[name]],
        name,
        positive = grepl("_sd$", name)
      )
    }
  }

  resolved
}

.lpmec_validate_joint2_prior_scalar <- function(value, name, positive = FALSE) {
  if (!is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      (positive && value <= 0)) {
    qualifier <- if (positive) "a single positive finite numeric value" else
      "a single finite numeric value"
    stop("mcmc_control$joint2_prior$", name, " must be ", qualifier, ".")
  }
  as.numeric(value)
}

.lpmec_resolve_joint2_prior <- function(joint2_prior = list()) {
  if (is.null(joint2_prior)) {
    joint2_prior <- list()
  }
  if (!is.list(joint2_prior)) {
    stop("mcmc_control$joint2_prior must be a list.")
  }

  valid_names <- c("lambda_mean", "lambda_sd", "psi_shape", "psi_scale")
  unknown_names <- setdiff(names(joint2_prior), valid_names)
  if (length(unknown_names) > 0L) {
    stop("Unknown mcmc_control$joint2_prior field(s): ",
         paste(unknown_names, collapse = ", "), ".")
  }

  resolved <- list(
    lambda_mean = 0,
    lambda_sd = 2,
    psi_shape = 0.0005,
    psi_scale = 0.0005
  )

  for (name in valid_names) {
    if (!is.null(joint2_prior[[name]])) {
      resolved[[name]] <- .lpmec_validate_joint2_prior_scalar(
        joint2_prior[[name]],
        name,
        positive = name != "lambda_mean"
      )
    }
  }

  resolved
}

.lpmec_validate_binary_observables <- function(observables, label = "'observables'") {
  obs_raw <- unlist(observables, use.names = FALSE)
  obs_numeric <- suppressWarnings(as.numeric(as.character(obs_raw)))
  if (length(obs_numeric) == 0L ||
      any(is.na(obs_numeric)) ||
      !all(obs_numeric %in% c(0, 1))) {
    stop(label, " must contain only binary 0/1 values for estimation_method = 'mcmc_joint2'.")
  }
  invisible(TRUE)
}

.lpmec_validate_mcmc_joint2_inputs <- function(Y, observables, ordinal, mcmc_control) {
  if (isTRUE(ordinal)) {
    stop("estimation_method = 'mcmc_joint2' currently supports binary 0/1 observables only; ordinal = TRUE is not supported.")
  }
  if (!identical(mcmc_control$backend, "numpyro")) {
    stop("estimation_method = 'mcmc_joint2' requires mcmc_control$backend = 'numpyro'; the 'pscl' backend is not supported.")
  }
  if (any(!is.finite(Y))) {
    stop("estimation_method = 'mcmc_joint2' requires finite, non-missing values in 'Y'.")
  }
  .lpmec_validate_binary_observables(observables)
  invisible(TRUE)
}

.lpmec_mcmc_joint2_bayesian_slopes <- function(lambda_y1_draws, theta_draws) {
  theta_draws <- as.matrix(theta_draws)
  lambda_y1_draws <- as.numeric(lambda_y1_draws)

  if (ncol(theta_draws) != length(lambda_y1_draws)) {
    stop("'lambda_y1_draws' must have one value for each column of 'theta_draws'.")
  }

  theta_draw_sds <- apply(theta_draws, 2, stats::sd)
  inner_draws <- lambda_y1_draws * theta_draw_sds
  posterior_mean_theta <- rowMeans(theta_draws)
  outer_draws <- lambda_y1_draws * stats::sd(posterior_mean_theta)

  list(
    inner_draws = inner_draws,
    outer_draws = outer_draws,
    bayesian_ols_coef_inner_normed = mean(inner_draws),
    bayesian_ols_se_inner_normed = stats::sd(inner_draws),
    bayesian_ols_coef_outer_normed = mean(outer_draws),
    bayesian_ols_se_outer_normed = stats::sd(outer_draws)
  )
}

.lpmec_numpyro_array <- function(x) {
  as.array(lpmec_env$np$array(x))
}

.lpmec_mcmc_joint2_extract_theta <- function(theta) {
  theta_array <- .lpmec_numpyro_array(theta)
  theta_dim <- dim(theta_array)

  if (length(theta_dim) != 3L) {
    stop("Unexpected NumPyro theta draw shape for mcmc_joint2.")
  }

  do.call(cbind, lapply(seq_len(theta_dim[1L]), function(chain_i) {
    chain_draws <- theta_array[chain_i, , , drop = FALSE]
    dim(chain_draws) <- theta_dim[2L:3L]
    t(chain_draws)
  }))
}

.lpmec_mcmc_joint2_extract_scalar_draws <- function(x) {
  x_array <- .lpmec_numpyro_array(x)
  x_dim <- dim(x_array)
  if (length(x_dim) == 2L) {
    return(unlist(lapply(seq_len(x_dim[1L]), function(chain_i) {
      as.numeric(x_array[chain_i, ])
    }), use.names = FALSE))
  }
  as.numeric(x_array)
}

.lpmec_mcmc_joint2_diagnostics <- function(sampler, posterior_draws, theta_draws) {
  n_total_draws <- ncol(theta_draws)

  ess_values <- try(as.numeric(.lpmec_numpyro_array(
    lpmec_env$numpyro$diagnostics$effective_sample_size(posterior_draws$theta)
  )), silent = TRUE)
  if (inherits(ess_values, "try-error")) {
    ess_values <- NA_real_
  }

  rhat_values <- try(as.numeric(.lpmec_numpyro_array(
    lpmec_env$numpyro$diagnostics$gelman_rubin(posterior_draws$theta)
  )), silent = TRUE)
  if (inherits(rhat_values, "try-error")) {
    rhat_values <- NA_real_
  }

  finite_ess <- ess_values[is.finite(ess_values)]
  finite_rhat <- rhat_values[is.finite(rhat_values)]

  extra_fields <- try(sampler$get_extra_fields(group_by_chain = TRUE), silent = TRUE)
  divergent <- NA_real_
  accept_prob <- NA_real_
  if (!inherits(extra_fields, "try-error")) {
    divergent <- try(sum(as.logical(.lpmec_numpyro_array(extra_fields$diverging))), silent = TRUE)
    if (inherits(divergent, "try-error")) {
      divergent <- NA_real_
    }
    accept_prob <- try(mean(as.numeric(.lpmec_numpyro_array(extra_fields$accept_prob)), na.rm = TRUE), silent = TRUE)
    if (inherits(accept_prob, "try-error") || !is.finite(accept_prob)) {
      accept_prob <- NA_real_
    }
  }

  list(
    ability_mean_ess_pct = if (length(finite_ess) > 0L) 100 * mean(finite_ess) / n_total_draws else NA_real_,
    ability_min_ess_pct = if (length(finite_ess) > 0L) 100 * min(finite_ess) / n_total_draws else NA_real_,
    max_rhat = if (length(finite_rhat) > 0L) max(finite_rhat) else NA_real_,
    num_divergent = as.numeric(divergent),
    mean_accept_prob = as.numeric(accept_prob)
  )
}

.lpmec_run_numpyro_mcmc_joint2 <- function(Y,
                                           observables,
                                           mcmc_control,
                                           conda_env = "lpmec",
                                           conda_env_required = FALSE) {
  if (!"jax" %in% ls(envir = lpmec_env)) {
    initialize_jax(conda_env, conda_env_required)
  }
  if (!"jax_scipy_special" %in% ls(envir = lpmec_env)) {
    lpmec_env$jax_scipy_special <- reticulate::import("jax.scipy.special")
  }

  joint2_prior <- .lpmec_resolve_joint2_prior(mcmc_control$joint2_prior)

  lpmec_env$numpyro$set_host_device_count(mcmc_control$n_chains)
  N <- ai(nrow(observables))
  K <- ai(ncol(observables))

  MixedFactorModel <- function(X, Y) {
    with(lpmec_env$numpyro$plate("rows", X$shape[[1]], dim = -1L), {
      theta <- lpmec_env$numpyro$sample("theta", lpmec_env$dist$Normal(0, 1))
    })

    with(lpmec_env$numpyro$plate("columns", X$shape[[2]], dim = -1L), {
      lambda_item_intercept <- lpmec_env$numpyro$sample(
        "lambda_item_intercept",
        lpmec_env$dist$Normal(joint2_prior$lambda_mean, joint2_prior$lambda_sd)
      )
    })

    lambda_item_loading_first <- lpmec_env$numpyro$sample(
      "lambda_item_loading_first",
      lpmec_env$dist$TruncatedNormal(
        loc = joint2_prior$lambda_mean,
        scale = joint2_prior$lambda_sd,
        low = 0
      )
    )
    if (K > 1L) {
      with(lpmec_env$numpyro$plate("columns_free", K - 1L, dim = -1L), {
        lambda_item_loading_free <- lpmec_env$numpyro$sample(
          "lambda_item_loading_free",
          lpmec_env$dist$Normal(joint2_prior$lambda_mean, joint2_prior$lambda_sd)
        )
      })
      lambda_item_loading <- lpmec_env$numpyro$deterministic(
        "lambda_item_loading",
        lpmec_env$jnp$concatenate(
          list(
            lpmec_env$jnp$expand_dims(lambda_item_loading_first, 0L),
            lambda_item_loading_free
          ),
          axis = 0L
        )
      )
    } else {
      lambda_item_loading <- lpmec_env$numpyro$deterministic(
        "lambda_item_loading",
        lpmec_env$jnp$expand_dims(lambda_item_loading_first, 0L)
      )
    }

    lambda_y0 <- lpmec_env$numpyro$sample(
      "lambda_y0",
      lpmec_env$dist$Normal(joint2_prior$lambda_mean, joint2_prior$lambda_sd)
    )
    lambda_y1 <- lpmec_env$numpyro$sample(
      "lambda_y1",
      lpmec_env$dist$Normal(joint2_prior$lambda_mean, joint2_prior$lambda_sd)
    )
    psi_y <- lpmec_env$numpyro$sample(
      "psi_y",
      lpmec_env$dist$InverseGamma(joint2_prior$psi_shape, joint2_prior$psi_scale)
    )
    sigma_y <- lpmec_env$numpyro$deterministic("sigma_y", lpmec_env$jnp$sqrt(psi_y))

    theta_col <- lpmec_env$jnp$expand_dims(theta, 1L)
    item_probs <- lpmec_env$jax_scipy_special$ndtr(
      lambda_item_intercept + lambda_item_loading * theta_col
    )
    with(lpmec_env$numpyro$plate("rows_obs", X$shape[[1]], dim = -2L), {
      with(lpmec_env$numpyro$plate("columns_obs", X$shape[[2]], dim = -1L), {
        lpmec_env$numpyro$sample("Xlik", lpmec_env$dist$Bernoulli(probs = item_probs), obs = X)
      })
    })

    Y_mu <- lambda_y0 + lambda_y1 * theta
    lpmec_env$numpyro$sample("Ylik", lpmec_env$dist$Normal(Y_mu, sigma_y), obs = Y)
  }

  kernel <- lpmec_env$numpyro$infer$NUTS(
    MixedFactorModel,
    max_tree_depth = ai(8),
    target_accept_prob = 0.85
  )

  progress_bar <- TRUE
  if (!is.null(mcmc_control$progress_bar)) {
    progress_bar <- isTRUE(mcmc_control$progress_bar)
  }

  sampler <- lpmec_env$numpyro$infer$MCMC(
    kernel,
    num_warmup = mcmc_control$n_samples_warmup,
    num_samples = mcmc_control$n_samples_mcmc,
    thinning = mcmc_control$n_thin_by,
    chain_method = mcmc_control$chain_method,
    num_chains = mcmc_control$n_chains,
    jit_model_args = TRUE,
    progress_bar = progress_bar
  )

  ddtype_ <- lpmec_env$jnp$float32
  lpmec_env$jax$config$update("jax_enable_x64", FALSE)
  rng_seed <- if (!is.null(mcmc_control$seed)) {
    ai(mcmc_control$seed)
  } else {
    ai(stats::runif(1, 0, 10000))
  }

  t0_ <- Sys.time()
  sampler$run(
    lpmec_env$jax$random$PRNGKey(rng_seed),
    X = lpmec_env$jnp$array(as.matrix(observables))$astype(ddtype_),
    Y = lpmec_env$jnp$array(as.numeric(Y))$astype(ddtype_),
    extra_fields = c("diverging", "accept_prob")
  )
  message(sprintf(
    "\n MCMC joint2 Runtime: %.3f min",
    as.numeric(difftime(Sys.time(), t0_, units = "secs")) / 60
  ))

  posterior_draws <- sampler$get_samples(group_by_chain = TRUE)
  theta_draws <- .lpmec_mcmc_joint2_extract_theta(posterior_draws$theta)
  lambda_y1_draws <- .lpmec_mcmc_joint2_extract_scalar_draws(posterior_draws$lambda_y1)
  slopes <- .lpmec_mcmc_joint2_bayesian_slopes(lambda_y1_draws, theta_draws)
  diagnostics <- .lpmec_mcmc_joint2_diagnostics(sampler, posterior_draws, theta_draws)

  list(
    x_est = as.matrix(scale(rowMeans(theta_draws))),
    bayesian_ols_coef_inner_normed = slopes$bayesian_ols_coef_inner_normed,
    bayesian_ols_se_inner_normed = slopes$bayesian_ols_se_inner_normed,
    bayesian_ols_coef_outer_normed = slopes$bayesian_ols_coef_outer_normed,
    bayesian_ols_se_outer_normed = slopes$bayesian_ols_se_outer_normed,
    mcmc_joint2_ability_mean_ess_pct = diagnostics$ability_mean_ess_pct,
    mcmc_joint2_ability_min_ess_pct = diagnostics$ability_min_ess_pct,
    mcmc_joint2_max_rhat = diagnostics$max_rhat,
    mcmc_joint2_num_divergent = diagnostics$num_divergent,
    mcmc_joint2_mean_accept_prob = diagnostics$mean_accept_prob
  )
}
