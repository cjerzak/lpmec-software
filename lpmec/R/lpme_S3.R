#' Summary method for lpmec_onerun objects
#'
#' Provides a summary of single-run LPMEC model results including OLS, IV,
#' and corrected coefficient estimates.
#'
#' @param object An object of class \code{lpmec_onerun} returned by \code{\link{lpmec_onerun}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame containing coefficient estimates and standard errors,
#'   returned invisibly. The data frame has rows for OLS, IV, Corrected IV,
#'   and Corrected OLS estimates.
#'
#' @seealso \code{\link{lpmec_onerun}}, \code{\link{print.lpmec_onerun}}, \code{\link{plot.lpmec_onerun}}
#'
#' @export
summary.lpmec_onerun <- function(object, ...) {
  coef_df <- data.frame(
    Estimate = c(object$ols_coef, object$iv_coef, object$corrected_iv_coef, object$corrected_ols_coef),
    SE = c(object$ols_se, object$iv_se, object$corrected_iv_se, object$corrected_ols_se),
    row.names = c("OLS", "IV", "Corrected IV", "Corrected OLS")
  )

  cat("Single-Run LPMEC Model Summary\n")
  cat("==============================\n")
  print(coef_df)
  invisible(coef_df)
}

#' Print method for lpmec_onerun objects
#'
#' Prints a concise summary of single-run LPMEC model results.
#'
#' @param x An object of class \code{lpmec_onerun} returned by \code{\link{lpmec_onerun}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object \code{x}, returned invisibly.
#'
#' @seealso \code{\link{lpmec_onerun}}, \code{\link{summary.lpmec_onerun}}, \code{\link{plot.lpmec_onerun}}
#'
#' @export
print.lpmec_onerun <- function(x, ...) {
  cat("Single-Run LPMEC Results\n")
  cat("------------------------\n")
  cat(sprintf("Uncorrected Coefficient (OLS): %.3f (SE: %.3f)\n", x$ols_coef, x$ols_se))
  cat(sprintf("Corrected Coefficient: %.3f (SE: %.3f)\n", x$corrected_ols_coef, x$corrected_ols_se))
  cat("Use summary() for detailed results.\n")
  invisible(x)
}

#' Plot method for lpmec_onerun objects
#'
#' Creates a scatter plot comparing the two split-half latent variable estimates.
#'
#' @param x An object of class \code{lpmec_onerun} returned by \code{\link{lpmec_onerun}}.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}.
#'
#' @return No return value, called for side effects (creates a plot).
#'
#' @seealso \code{\link{lpmec_onerun}}, \code{\link{summary.lpmec_onerun}}, \code{\link{print.lpmec_onerun}}
#'
#' @export
plot.lpmec_onerun <- function(x, ...) {
  plot(x$x_est1, x$x_est2,
       xlab = "First Latent Estimate", ylab = "Second Latent Estimate",
       main = "Single-Run Latent Estimates", pch = 19, ...)
  abline(a = 0, b = 1, col = "blue", lty = 2)
}


#' Summary method for lpmec objects
#'
#' Provides a comprehensive summary of bootstrapped LPMEC model results including
#' OLS, IV, corrected, and Bayesian coefficient estimates with confidence intervals.
#'
#' @param object An object of class \code{lpmec} returned by \code{\link{lpmec}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame containing coefficient estimates, standard errors, and
#'   confidence intervals, returned invisibly. The data frame has rows for OLS,
#'   IV, Corrected IV, Corrected OLS, and Bayesian OLS estimates.
#'
#' @seealso \code{\link{lpmec}}, \code{\link{print.lpmec}}, \code{\link{plot.lpmec}}
#'
#' @export
summary.lpmec <- function(object, ...) {
  coef_df <- data.frame(
    Estimate = c(object$ols_coef, object$iv_coef, object$corrected_iv_coef,
                 object$corrected_ols_coef, object$bayesian_ols_coef_outer_normed,
                 object$bayesian_ols_coef_inner_normed),
    SE = c(object$ols_se, object$iv_se, object$corrected_iv_se,
           object$corrected_ols_se, object$bayesian_ols_se_outer_normed,
           object$bayesian_ols_se_inner_normed),
    CI_Lower = c(object$ols_lower, object$iv_lower, object$corrected_iv_lower,
                 object$corrected_ols_lower, object$bayesian_ols_lower_outer_normed,
                 object$bayesian_ols_lower_inner_normed),
    CI_Upper = c(object$ols_upper, object$iv_upper, object$corrected_iv_upper,
                 object$corrected_ols_upper, object$bayesian_ols_upper_outer_normed,
                 object$bayesian_ols_upper_inner_normed),
    row.names = c("OLS", "IV", "Corrected IV", "Corrected OLS",
                  "Bayesian OLS (Outer)", "Bayesian OLS (Inner)")
  )

  cat("Latent Predictor Measurement Error Correction (LPMEC) Model Summary\n")
  cat("====================================================================\n")
  if (!is.null(object$bootstrap_method)) {
    cat(sprintf(
      "Resampling: %s, m = %s (m/n = %.3f), CI = %s, replace = %s\n",
      object$bootstrap_method,
      object$boot_m,
      object$boot_m_ratio,
      object$boot_ci_type,
      object$boot_replace
    ))
    if (!is.null(object$bootstrap_success_rate) && is.finite(object$bootstrap_success_rate)) {
      cat(sprintf("Bootstrap success rate: %.3f\n", object$bootstrap_success_rate))
    }
  }
  print(coef_df)
  invisible(coef_df)
}

#' Print method for lpmec objects
#'
#' Prints a concise summary of bootstrapped LPMEC model results.
#'
#' @param x An object of class \code{lpmec} returned by \code{\link{lpmec}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object \code{x}, returned invisibly.
#'
#' @seealso \code{\link{lpmec}}, \code{\link{summary.lpmec}}, \code{\link{plot.lpmec}}
#'
#' @export
print.lpmec <- function(x, ...) {
  cat("Latent Predictor Measurement Error Correction (LPMEC) Model Results\n")
  cat("-------------------------------------------------------------------\n")
  if (!is.null(x$bootstrap_method)) {
    cat(sprintf(
      "Resampling: %s, m = %s, CI = %s\n",
      x$bootstrap_method,
      x$boot_m,
      x$boot_ci_type
    ))
  }
  cat(sprintf("Uncorrected Coefficient (OLS): %.3f (SE: %.3f)\n", x$ols_coef, x$ols_se))
  cat(sprintf("Corrected Coefficient: %.3f (SE: %.3f)\n", x$corrected_iv_coef, x$corrected_iv_se))
  cat(sprintf("Bayesian OLS (Outer): %.3f (SE: %.3f)\n", x$bayesian_ols_coef_outer_normed,
              x$bayesian_ols_se_outer_normed))
  cat("Use summary() for detailed results.\n")
  invisible(x)
}

#' Plot method for lpmec objects
#'
#' Creates visualizations of LPMEC model results. Can plot either the latent
#' variable estimates or the bootstrap distribution of coefficients.
#'
#' @param x An object of class \code{lpmec} returned by \code{\link{lpmec}}.
#' @param type Character string specifying the plot type. Either \code{"latent"}
#'   (default) for a scatter plot of split-half latent estimates, or
#'   \code{"coefficients"} for a density plot of bootstrap coefficient estimates.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}} or
#'   \code{\link[stats]{density}}.
#'
#' @return No return value, called for side effects (creates a plot).
#'
#' @seealso \code{\link{lpmec}}, \code{\link{summary.lpmec}}, \code{\link{print.lpmec}}
#'
#' @export
plot.lpmec <- function(x, type = "latent", ...) {
  if (type == "latent") {
    plot(x$x_est1, x$x_est2,
         xlab = "First Latent Estimate", ylab = "Second Latent Estimate",
         main = "Latent Variable Estimates", pch = 19, ...)
    abline(a = 0, b = 1, col = "red", lty = 2)
  } else if (type == "coefficients") {
    if (is.null(x$Intermediary_corrected_iv_coef) ||
        is.null(x$Intermediary_BootIndex)) {
      stop("Coefficient plots require lpmec(..., return_intermediaries = TRUE).")
    }
    boot_coefs <- as.numeric(x$Intermediary_corrected_iv_coef[
      x$Intermediary_BootIndex != 1
    ])
    boot_coefs <- boot_coefs[is.finite(boot_coefs)]
    if (length(boot_coefs) < 2L) {
      stop("Coefficient plots require bootstrap draws; run lpmec(..., n_boot >= 1, return_intermediaries = TRUE).")
    }
    plot(density(boot_coefs), main = "Bootstrap Distribution of Corrected IV Coefficient",
         xlab = "Coefficient Value", ...)
  } else {
    stop("Invalid plot type. Choose 'latent' or 'coefficients'.")
  }
}

#' Summary method for lpmec_multivariate_onerun objects
#'
#' @param object An object of class \code{lpmec_multivariate_onerun}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame of latent-predictor coefficient estimates.
#'
#' @export
summary.lpmec_multivariate_onerun <- function(object, ...) {
  coef_df <- data.frame(
    OLS = object$ols_coef,
    IV = object$iv_coef,
    Corrected_IV = object$corrected_iv_coef,
    Split_Correlation = object$split_correlation,
    First_Stage_F = object$first_stage_fstat,
    row.names = object$latent_names
  )

  cat("Single-Run Multivariate LPMEC Summary\n")
  cat("=====================================\n")
  print(coef_df)
  invisible(coef_df)
}

#' Print method for lpmec_multivariate_onerun objects
#'
#' @param x An object of class \code{lpmec_multivariate_onerun}.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object \code{x}, returned invisibly.
#'
#' @export
print.lpmec_multivariate_onerun <- function(x, ...) {
  cat("Single-Run Multivariate LPMEC Results\n")
  cat("-------------------------------------\n")
  cat(sprintf("Latent predictors: %s\n", paste(x$latent_names, collapse = ", ")))
  cat("Use summary() for coefficient details.\n")
  invisible(x)
}

#' Summary method for lpmec_multivariate objects
#'
#' @param object An object of class \code{lpmec_multivariate}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame of aggregated latent-predictor coefficient estimates.
#'
#' @export
summary.lpmec_multivariate <- function(object, ...) {
  coef_df <- data.frame(
    OLS = object$ols_coef,
    OLS_SE = object$ols_se,
    Corrected_IV = object$corrected_iv_coef,
    Corrected_IV_SE = object$corrected_iv_se,
    Corrected_IV_Lower = object$corrected_iv_lower,
    Corrected_IV_Upper = object$corrected_iv_upper,
    Split_Correlation = object$split_correlation,
    First_Stage_F = object$first_stage_fstat,
    row.names = object$latent_names
  )

  cat("Multivariate LPMEC Summary\n")
  cat("==========================\n")
  print(coef_df)
  invisible(coef_df)
}

#' Print method for lpmec_multivariate objects
#'
#' @param x An object of class \code{lpmec_multivariate}.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object \code{x}, returned invisibly.
#'
#' @export
print.lpmec_multivariate <- function(x, ...) {
  cat("Multivariate LPMEC Results\n")
  cat("--------------------------\n")
  cat(sprintf("Latent predictors: %s\n", paste(x$latent_names, collapse = ", ")))
  cat("Use summary() for coefficient details.\n")
  invisible(x)
}
