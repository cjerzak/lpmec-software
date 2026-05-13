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
