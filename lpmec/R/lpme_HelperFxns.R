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
