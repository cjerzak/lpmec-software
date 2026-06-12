#' lpmec
#'
#' Implements latent variable models with measurement error correction
#'
#' @param Y A vector of observed outcome variables
#' @param observables A matrix of observable indicators used to estimate the latent variable
#' @param orientation_signs (optional) A numeric vector of length equal to the number of columns in `observables`, containing 1 or -1 to indicate the desired orientation of each column. If provided, each column of `observables` will be oriented by this sign before analysis. Default is NULL (no orientation applied).
#' @param observables_groupings A vector specifying groupings for the observable indicators. Default is column names of observables.
#' @param make_observables_groupings Logical. If TRUE, creates dummy variables for each level of the observable indicators. Default is FALSE.
#' @param n_boot Non-negative integer. Number of bootstrap iterations. Use
#'   \code{0} to disable bootstrap resampling and fit only the original sample.
#'   Default is 32.
#' @param n_partition Positive integer. Number of split-half partitions for each
#'   bootstrap iteration. When \code{n_boot = 0}, this still controls how many
#'   original-sample partition runs are aggregated. Default is 10.
#' @param partition_aggregation Aggregation strategy for combining estimates across
#'   partitions within each bootstrap iteration. Default is \code{"median"}. Options
#'   are \code{"median"}, \code{"winsorized_mean"}, \code{"trimmed_mean"}, or a
#'   custom function that accepts a numeric vector and returns one numeric value.
#' @param partition_aggregation_probs Numeric vector of length 2 used by
#'   \code{"winsorized_mean"} and \code{"trimmed_mean"}. For winsorization, values
#'   are clipped to these quantiles before averaging. For trimming, values outside
#'   these quantiles are dropped before averaging. Default is \code{c(0.01, 0.99)}.
#' @param boot_basis Vector of indices or grouping variable for stratified bootstrap. Default is 1:length(Y).
#' @param bootstrap_method Resampling method for uncertainty. Options are
#'   \code{"n_out_of_n"}, \code{"m_out_of_n"}, \code{"subsampling"}, and
#'   \code{"auto"}. The default \code{"n_out_of_n"} preserves historical row
#'   bootstrap behavior. Use \code{"subsampling"} or \code{"m_out_of_n"} with
#'   \code{boot_ci_type = "root"} for the formal nonsmooth median route.
#' @param boot_m Optional exact m for m-out-of-n bootstrap or subsampling.
#' @param boot_m_rule Rule used when \code{boot_m = NULL}. \code{"power"} uses
#'   \code{floor(n^boot_m_exponent)}; \code{"fixed"} requires \code{boot_m};
#'   \code{"grid_stability"} records a candidate grid and currently selects the
#'   grid value nearest the power-rule value.
#' @param boot_m_exponent Exponent used by \code{boot_m_rule = "power"}.
#'   Default is \code{0.70}.
#' @param boot_m_grid Optional candidate grid for m-sensitivity diagnostics.
#' @param boot_m_replace Optional logical override for row replacement in
#'   m-out-of-n/subsampling. By default, replacement is used for
#'   \code{"n_out_of_n"} and \code{"m_out_of_n"}, and not used for
#'   \code{"subsampling"}.
#' @param boot_ci_type Confidence interval type. \code{"auto"} uses percentile
#'   intervals for \code{"n_out_of_n"} and root-scaled intervals for m < n.
#'   \code{"root_calibrated"} uses nested subsampling to calibrate the root
#'   interval tail cutoffs. \code{"rbc"} uses finite-item-count robust bias
#'   correction with root-scaled intervals.
#' @param boot_alpha Confidence interval tail probability. Default is 0.05.
#' @param boot_rate Rate used by root-scaled intervals. The current formal path
#'   uses \code{"sqrt_n"}; \code{"custom"} requires \code{boot_tau}.
#' @param boot_tau Optional function used when \code{boot_rate = "custom"}.
#' @param boot_calibration_n Positive integer. Number of outer calibration
#'   subsamples used when \code{boot_ci_type = "root_calibrated"}. Default is
#'   25.
#' @param boot_calibration_inner_n_boot Optional positive integer. Number of
#'   inner resamples within each calibration subsample. Defaults to
#'   \code{n_boot}.
#' @param boot_calibration_cut_grid Candidate one-sided tail probabilities for
#'   nested root interval calibration. Values must be greater than \code{0} and
#'   less than or equal to \code{boot_alpha}. Default is
#'   \code{c(0.005, 0.010, 0.015, 0.020, 0.025, 0.035, 0.050)}.
#' @param boot_calibration_tail Calibration mode. \code{"separate"} calibrates
#'   lower and upper one-sided root intervals separately; \code{"equal_tail"}
#'   chooses one common equal-tail cutoff.
#' @param boot_calibration_seed Optional seed for the nested calibration stage.
#' @param boot_rbc_item_counts Optional integer vector of observable-group
#'   counts used to estimate the leading finite-M bias when
#'   \code{boot_ci_type = "rbc"}. Defaults to a fixed grid based on fractions
#'   of the full number of observable groups.
#' @param boot_rbc_n_subsets Positive integer. Number of item subsets drawn at
#'   each value of \code{boot_rbc_item_counts}. Used only when
#'   \code{boot_ci_type = "rbc"}. Default is 10.
#' @param boot_rbc_seed Optional seed for the item-subset schedule used by
#'   \code{boot_ci_type = "rbc"}.
#' @param partition_set Optional user-supplied fixed partition list. Each element
#'   must contain \code{split1_names}, \code{split2_names}, and optionally
#'   \code{partition_id}.
#' @param fix_partitions Logical. If \code{TRUE}, draw or accept the finite
#'   partition set once and hold it fixed across original and resampled fits.
#' @param seed Optional seed used for reproducible partitions and resamples.
#' @param ordinal Logical indicating whether the observable indicators are ordinal (TRUE) or binary (FALSE).
#' @param return_intermediaries Logical. If TRUE, returns intermediate results. Default is TRUE.
#' @param estimation_method Character specifying the estimation approach. Options include:
#' \itemize{
#' \item "em" (default): Uses expectation-maximization via \code{emIRT} package. Supports both binary (via \code{emIRT::binIRT}) and ordinal (via \code{emIRT::ordIRT}) indicators.
#' \item "pca": First principal component of observables.
#' \item "averaging": Uses feature averaging.
#' \item "mcmc": Markov Chain Monte Carlo estimation using either \code{pscl::ideal} (R backend) or \code{numpyro} (Python backend)
#' \item "mcmc_joint": Joint Bayesian model that simultaneously estimates latent variables and outcome relationship using \code{numpyro}
#' \item "mcmc_joint2": NumPyro mixed factor-analysis benchmark with binary indicators and continuous \code{Y} in one factor model
#' \item "mcmc_overimputation": Two-stage MCMC approach with measurement error correction via over-imputation
#' \item "custom": In this case, latent estimation performed using \code{latent_estimation_fn}.
#' }
#' @param latent_estimation_fn Custom function for estimating latent trait from \code{observables} if \code{estimation_method="custom"} (optional). The function should accept a matrix of observables (rows are observations) and return a numeric vector of length equal to the number of observations.
#' @param mcmc_control A list indicating parameter specifications if MCMC used.
#' \describe{
#'   \item{\code{backend}}{Character string indicating the MCMC engine to use.
#'     Valid options are \code{"pscl"} (default, uses the R-based \code{pscl::ideal} function)
#'     or \code{"numpyro"} (uses the Python numpyro package via reticulate).}
#'   \item{\code{n_samples_warmup}}{Integer specifying the number of warm-up (burn-in)
#'     iterations before samples are collected. Default is \code{500}.}
#'   \item{\code{n_samples_mcmc}}{Integer specifying the number of post-warmup MCMC
#'     iterations to retain. Default is \code{1000}.}
#'   \item{\code{batch_size}}{Integer row subsample size used when
#'     \code{subsample_method = "batch"} with the NumPyro backend. Must be
#'     between \code{1} and \code{nrow(observables) - 1}. Default is
#'     \code{512}.}
#'   \item{\code{subsample_method}}{Character string for NumPyro likelihood
#'     evaluation. Use \code{"full"} (default) for all rows or \code{"batch"}
#'     for experimental HMCECS row subsampling with \code{batch_size}.}
#'   \item{\code{chain_method}}{Character string passed to numpyro specifying how to run
#'     multiple chains. Options: \code{"parallel"} (default), \code{"sequential"},
#'     or \code{"vectorized"}.}
#'   \item{\code{anchor_parameter_id}}{Optional 1-based observable-column index
#'     used by NumPyro MCMC backends to anchor item difficulty orientation. If
#'     omitted or \code{NULL}, automatic orientation is used.}
#'   \item{\code{n_thin_by}}{Integer indicating the thinning factor for MCMC samples.
#'     Default is \code{1}.}
#'   \item{\code{n_chains}}{Integer specifying the number of parallel MCMC chains to run.
#'     Default is \code{2}.}
#'   \item{\code{outcome_prior}}{List controlling \code{"mcmc_joint"} outcome-model
#'     priors for the NumPyro backend. By default, \code{calibration = "data"}
#'     centers the intercept prior at \code{mean(Y)} and scales intercept, slope,
#'     and residual-sigma priors by \code{sd(Y)}. Use \code{calibration = "legacy"}
#'     to restore the previous unit-scale priors, or provide numeric overrides for
#'     \code{intercept_mean}, \code{intercept_sd}, \code{slope_mean},
#'     \code{slope_sd}, and \code{sigma_sd}. The optional \code{scale_floor}
#'     sets the minimum scale used for data-calibrated priors.}
#'   \item{\code{joint2_prior}}{List controlling \code{"mcmc_joint2"}
#'     priors. Defaults are \code{lambda_mean = 0}, \code{lambda_sd = 2},
#'     \code{psi_shape = 0.0005}, and \code{psi_scale = 0.0005}.
#'     For item loadings, \code{lambda_mean} and \code{lambda_sd}
#'     parameterize the raw loading scale before the positive \code{softplus}
#'     transform; all item loadings are positive by default.}
#' }
#' @param conda_env A character string specifying the name of the conda environment to use
#'   via \code{reticulate}. Default is \code{"lpmec"}.
#' @param conda_env_required A logical indicating whether the specified conda environment
#'   must be strictly used. If \code{TRUE}, an error is thrown if the environment is not found.
#'   Default is \code{FALSE}.
#'
#' @return A list containing various estimates and statistics (in snake_case):
#' \itemize{
#'   \item Naive, IV, corrected IV, and corrected OLS estimates:
#'     \code{ols_*}, \code{iv_*}, \code{corrected_iv_*}, and
#'     \code{corrected_ols_*}. Bootstrap uncertainty summaries use suffixes
#'     \code{_se}, \code{_lower}, \code{_upper}, and \code{_tstat} where
#'     applicable.
#'   \item \code{var_est_split} and \code{var_est_split_se}: Aggregated
#'     split-half measurement-error variance and, when bootstrap draws are
#'     available, its bootstrap standard error.
#'   \item \code{bayesian_ols_*_outer_normed} and
#'     \code{bayesian_ols_*_inner_normed}: MCMC coefficient summaries. The
#'     \code{*_parametric} standard-error fields retain within-run posterior
#'     uncertainty, while the non-parametric standard-error and interval fields
#'     summarize bootstrap variation.
#'   \item \code{m_stage_1_erv*} and \code{m_reduced_erv*}: Extreme robustness
#'     values and bootstrap uncertainty summaries for the first-stage and
#'     reduced-form regressions.
#'   \item \code{mcmc_joint2_*}: NumPyro \code{"mcmc_joint2"} diagnostics,
#'     including effective-sample-size percentages, maximum R-hat, divergent
#'     transitions, mean accept probability, and orientation diagnostics.
#'   \item \code{x_est1} and \code{x_est2}: Split-half latent variable
#'     estimates from the original sample.
#'   \item \code{boot_rbc_diagnostics}, \code{boot_rbc_raw_aggregates}, and
#'     \code{boot_rbc_pilot_aggregates}: Robust-bias-correction diagnostics
#'     returned when \code{boot_ci_type = "rbc"}.
#'   \item \code{Intermediary_*}: Per-run original-sample and bootstrap
#'     outputs, returned only when \code{return_intermediaries = TRUE}.
#' }
#'
#' @details
#' This function implements a latent variable analysis with measurement error correction.
#' It fits the original sample and, when \code{n_boot >= 1}, performs bootstrap
#' resampling for uncertainty estimates. Each original or bootstrap sample is
#' analyzed with one or more split-half partitions. For each partition,
#' it calls the \code{lpmec_onerun} function to estimate latent variables and apply various correction methods.
#' The results are then aggregated across partitions and bootstrap iterations to produce final estimates
#' and, when bootstrap draws are available, bootstrap standard errors.
#'
#' For \code{partition_aggregation = "median"}, the finite-partition median is
#' a nonsmooth aggregation rule. The ordinary n-out-of-n row bootstrap remains
#' available through \code{bootstrap_method = "n_out_of_n"} for backward
#' compatibility. The formal nonsmooth-functional route is
#' \code{bootstrap_method = "subsampling"} or \code{"m_out_of_n"} with
#' \code{boot_ci_type = "root"} or \code{"root_calibrated"}. In that route,
#' the same realized partition set is held fixed across the original sample
#' and all resamples, each resample reruns the full latent-score and correction
#' pipeline, and confidence intervals invert the empirical distribution of
#' \code{sqrt(m) * (theta_boot - theta_hat)} at the original \code{sqrt(n)}
#' rate.
#'
#' With \code{boot_ci_type = "root_calibrated"}, \code{lpmec()} performs a
#' nested subsampling calibration of the root interval cutoffs. For each
#' calibration subsample of size \code{m}, it treats the full-sample estimate as
#' a pseudo-truth, builds inner root intervals from smaller subsamples, estimates
#' candidate one-sided coverage rates, and applies the largest candidate cutoff
#' whose estimated coverage reaches the nominal target. This is an asymptotic
#' resampling calibration, not a finite-sample coverage guarantee. Its usual
#' justification relies on scale separation for the outer and inner subsample
#' sizes.
#'
#' With \code{boot_ci_type = "rbc"}, \code{lpmec()} estimates a leading
#' finite-item-count bias by rerunning the estimator on a fixed, pre-specified
#' grid of observable-group subsets and fitting each target summary to
#' \code{a + b / M}. It subtracts \code{b / M} from the full-M estimate and
#' constructs root intervals from bootstrap draws of the same bias-corrected
#' statistic.
#'
#' @examples
#' \donttest{
#' # Generate some example data
#' set.seed(123)
#' Y <- rnorm(1000)
#' observables <- as.data.frame(matrix(sample(c(0,1), 1000*10, replace = TRUE), ncol = 10))
#'
#' # Run the bootstrapped analysis
#' results <- lpmec(Y = Y,
#'                  observables = observables,
#'                  n_boot = 10,    # small values for illustration only
#'                  n_partition = 5 # small for size
#'                  )
#'
#' # Use a winsorized mean across partitions
#' results_winsorized <- lpmec(Y = Y,
#'                             observables = observables,
#'                             n_boot = 10,
#'                             n_partition = 5,
#'                             partition_aggregation = "winsorized_mean")
#'
#' # View the corrected IV coefficient and its standard error
#' print(results)
#' }
#'
#' @references
#' Jerzak, C. T. and Jessee, S. A. (2025). Attenuation Bias with Latent Predictors.
#' arXiv:2507.22218 [stat.AP]. \url{https://arxiv.org/abs/2507.22218}
#'
#' Calonico, S., Cattaneo, M. D., and Farrell, M. H. (2018). On the Effect of
#' Bias Estimation on Coverage Accuracy in Nonparametric Inference. Journal of
#' the American Statistical Association, 113, 767--779.
#'
#' @export
#' @importFrom stats sd median na.omit

lpmec <- function(Y,
                  observables,
                  observables_groupings = colnames(observables),
                  orientation_signs = NULL,
                  make_observables_groupings = FALSE,
                  n_boot = 32L,
                  n_partition = 10L,
                  partition_aggregation = "median",
                  partition_aggregation_probs = c(0.01, 0.99),
                  boot_basis = 1:length(Y),
                  bootstrap_method = c("n_out_of_n", "m_out_of_n", "subsampling", "auto"),
                  boot_m = NULL,
                  boot_m_rule = c("power", "fixed", "grid_stability"),
                  boot_m_exponent = 0.70,
                  boot_m_grid = NULL,
                  boot_m_replace = NULL,
                  boot_ci_type = c("auto", "root", "root_calibrated", "percentile", "rbc"),
                  boot_alpha = 0.05,
                  boot_rate = c("sqrt_n", "custom"),
                  boot_tau = NULL,
                  boot_calibration_n = 25L,
                  boot_calibration_inner_n_boot = NULL,
                  boot_calibration_cut_grid = c(0.005, 0.010, 0.015, 0.020, 0.025, 0.035, 0.050),
                  boot_calibration_tail = c("separate", "equal_tail"),
                  boot_calibration_seed = NULL,
                  boot_rbc_item_counts = NULL,
                  boot_rbc_n_subsets = 10L,
                  boot_rbc_seed = NULL,
                  partition_set = NULL,
                  fix_partitions = TRUE,
                  seed = NULL,
                  return_intermediaries = TRUE,
                  ordinal = FALSE,
                  estimation_method = "em",
                  latent_estimation_fn = NULL,
                  mcmc_control = list(
                    backend = "pscl",  # will override to use NumPyro-based MCMC
                    n_samples_warmup = 500L,
                    n_samples_mcmc   = 1000L,
                    batch_size = 512L,
                    chain_method = "parallel",
                    subsample_method = "full",
                    anchor_parameter_id = NULL,
                    n_thin_by = 1L,
                    n_chains = 2L,
                    outcome_prior = list(calibration = "data"),
                    joint2_prior = list()),
                  conda_env = "lpmec",
                  conda_env_required = FALSE
                  ){
  boot_basis_supplied <- !missing(boot_basis)

  prepared_inputs <- .lpmec_prepare_common_inputs(
    Y = if (missing(Y)) NULL else Y,
    observables = if (missing(observables)) NULL else observables,
    observables_groupings = if (missing(observables_groupings)) NULL else observables_groupings,
    estimation_method = estimation_method,
    latent_estimation_fn = latent_estimation_fn,
    ordinal = ordinal,
    mcmc_control = mcmc_control,
    inform_partial_mcmc = TRUE,
    validate_joint2 = FALSE
  )
  Y <- prepared_inputs$Y
  observables <- prepared_inputs$observables
  observables_groupings <- prepared_inputs$observables_groupings
  estimation_method <- prepared_inputs$estimation_method
  latent_estimation_fn <- prepared_inputs$latent_estimation_fn
  ordinal <- prepared_inputs$ordinal
  mcmc_control <- prepared_inputs$mcmc_control
  if (!boot_basis_supplied) {
    boot_basis <- 1:length(Y)
  }

  is_whole_number <- function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x)
  }
  if (!is_whole_number(n_boot) || n_boot < 0) {
    stop("'n_boot' must be a single non-negative integer. Received: ", n_boot)
  }
  if (!is_whole_number(n_partition) || n_partition < 1) {
    stop("'n_partition' must be a single positive integer. Received: ", n_partition)
  }
  n_boot <- as.integer(n_boot)
  n_partition <- as.integer(n_partition)
  if (!is.numeric(boot_alpha) ||
      length(boot_alpha) != 1L ||
      !is.finite(boot_alpha) ||
      boot_alpha <= 0 ||
      boot_alpha >= 1) {
    stop("'boot_alpha' must be a single value in (0, 1).")
  }
  if (!is.null(boot_m_replace) &&
      (!is.logical(boot_m_replace) || length(boot_m_replace) != 1L || is.na(boot_m_replace))) {
    stop("'boot_m_replace' must be NULL or a single TRUE/FALSE value.")
  }
  if (!is.logical(fix_partitions) || length(fix_partitions) != 1L || is.na(fix_partitions)) {
    stop("'fix_partitions' must be a single TRUE/FALSE value.")
  }
  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed))) {
    stop("'seed' must be NULL or a single finite numeric value.")
  }
  boot_rate <- match.arg(boot_rate)

  # Validate boot_basis
  if (length(boot_basis) != length(Y)) {
    stop("'boot_basis' must have the same length as 'Y'. ",
         "Length of boot_basis: ", length(boot_basis), ", length of Y: ", length(Y))
  }

  if (!is.logical(return_intermediaries) || length(return_intermediaries) != 1) {
    stop("'return_intermediaries' must be a single logical value (TRUE or FALSE).")
  }

  # Resolve summarizing function for across-partition/boot aggregation
  theSumFxn <- .lpmec_resolve_partition_aggregation(
    partition_aggregation,
    partition_aggregation_probs
  )
  boot_spec <- .lpmec_resolve_bootstrap_method(
    bootstrap_method,
    partition_aggregation,
    boot_ci_type
  )
  bootstrap_method <- boot_spec$bootstrap_method
  boot_ci_type <- boot_spec$boot_ci_type
  boot_calibration_spec <- .lpmec_validate_root_calibration(
    boot_ci_type = boot_ci_type,
    bootstrap_method = bootstrap_method,
    n_boot = n_boot,
    boot_calibration_n = boot_calibration_n,
    boot_calibration_inner_n_boot = boot_calibration_inner_n_boot,
    boot_calibration_cut_grid = boot_calibration_cut_grid,
    boot_calibration_tail = boot_calibration_tail,
    boot_calibration_seed = boot_calibration_seed,
    alpha = boot_alpha
  )
  boot_calibration_n <- boot_calibration_spec$n
  boot_calibration_inner_n_boot <- boot_calibration_spec$inner_n_boot
  boot_calibration_cut_grid <- boot_calibration_spec$cut_grid
  boot_calibration_tail <- boot_calibration_spec$tail
  boot_calibration_seed <- boot_calibration_spec$seed
  if (n_boot >= 1L && n_boot < 199L) {
    warning(
      "n_boot < 199 gives coarse bootstrap confidence intervals; increase n_boot for interval estimation.",
      call. = FALSE
    )
  }
  if (!fix_partitions && bootstrap_method != "n_out_of_n") {
    warning(
      "Formal m-out-of-n/subsampling inference should fix the realized partition set. ",
      "Use fix_partitions = TRUE unless this is an exploratory run.",
      call. = FALSE
    )
  }
  if (!is.null(partition_set) && !fix_partitions) {
    warning("'partition_set' was supplied; using it despite fix_partitions = FALSE.", call. = FALSE)
    fix_partitions <- TRUE
  }
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(as.integer(seed))
  }

  boot_rbc_spec <- .lpmec_resolve_rbc_controls(
    boot_ci_type = boot_ci_type,
    observables_groupings = observables_groupings,
    boot_rbc_item_counts = boot_rbc_item_counts,
    boot_rbc_n_subsets = boot_rbc_n_subsets,
    boot_rbc_seed = boot_rbc_seed
  )

  m_spec <- .lpmec_resolve_m(
    n = length(Y),
    boot_m = boot_m,
    boot_m_rule = boot_m_rule,
    boot_m_exponent = boot_m_exponent,
    boot_m_grid = boot_m_grid,
    min_m = 10L,
    bootstrap_method = bootstrap_method
  )
  boot_m_resolved <- if (bootstrap_method == "n_out_of_n") length(Y) else m_spec$m
  boot_replace <- if (is.null(boot_m_replace)) {
    bootstrap_method %in% c("n_out_of_n", "m_out_of_n")
  } else {
    boot_m_replace
  }
  if (!boot_replace && boot_m_resolved > length(Y)) {
    stop("Cannot use boot_m > n when sampling without replacement.")
  }
  if (bootstrap_method != "n_out_of_n" &&
      estimation_method %in% c("em", "mcmc", "mcmc_joint", "mcmc_joint2", "mcmc_overimputation") &&
      boot_m_resolved < 20L &&
      is.null(boot_m)) {
    warning(
      "Resolved boot_m < 20 for an IRT/MCMC-style estimator; inspect finite-sample sensitivity.",
      call. = FALSE
    )
  }

  partition_list <- if (fix_partitions) {
    .lpmec_make_partitions(
      observables_groupings = observables_groupings,
      n_partition = n_partition,
      partition_set = partition_set,
      seed = NULL
    )
  } else {
    NULL
  }
  n_partition_actual <- if (fix_partitions) length(partition_list) else n_partition
  boot_rbc_plan <- if (isTRUE(boot_rbc_spec$enabled)) {
    .lpmec_make_rbc_plan(
      observables_groupings = observables_groupings,
      item_counts = boot_rbc_spec$item_counts,
      n_subsets = boot_rbc_spec$n_subsets,
      n_partition = n_partition_actual,
      seed = boot_rbc_spec$seed
    )
  } else {
    list()
  }
  if (isTRUE(boot_rbc_spec$enabled) && length(boot_rbc_plan) < 1L) {
    warning(
      "boot_ci_type = \"rbc\" has no valid lower item-count grid; returning uncorrected root intervals with RBC diagnostics.",
      call. = FALSE
    )
  }

  if (!is.null(orientation_signs)) {
    if (!is.numeric(orientation_signs) || length(orientation_signs) != ncol(observables)) {
      stop("orientation_signs must be a numeric vector of length equal to ncol(observables).")
    }
    if (!all(orientation_signs %in% c(1, -1))) {
      stop("orientation_signs must contain only 1 and -1.")
    }
    if(!all(na.omit(unlist(observables)) %in% c(0,1))){
      stop("Re-orientation in the non-binary case not yet implemented")
    }
    if(all(na.omit(unlist(observables)) %in% c(0,1))){
      colnames_observables <- colnames(observables)
      observables <- sapply(1:ncol(observables), function(d_){
        observables[, d_] <- orientation_signs[d_] * observables[, d_] + 
                                    (1 - orientation_signs[d_]) / 2
      })
      colnames(observables) <- colnames_observables
    }
  }

  if (estimation_method == "mcmc_joint2") {
    .lpmec_validate_mcmc_joint2_inputs(Y, observables, ordinal, mcmc_control)
    .lpmec_resolve_joint2_prior(mcmc_control$joint2_prior)
  }
  
  boot_indices_by_boot <- vector("list", n_boot + 1L)
  names(boot_indices_by_boot) <- as.character(seq_len(n_boot + 1L))
  for(booti_ in seq_len(n_boot + 1L)){
    if(booti_ == 1L){
      boot_indices <- seq_along(Y)
      current_m <- length(Y)
    } else {
      current_m <- boot_m_resolved
      boot_indices <- .lpmec_resample_indices(
        n = length(Y),
        m = current_m,
        replace = boot_replace,
        boot_basis = if (boot_basis_supplied) boot_basis else NULL
      )
    }
    boot_indices_by_boot[[as.character(booti_)]] <- boot_indices

    for(parti_ in seq_len(n_partition_actual)){
      current_partition <- if (fix_partitions) partition_list[[parti_]] else NULL
      current_partition_id <- if (fix_partitions) current_partition$partition_id else NULL
      message(sprintf("{booti_ %s of %s} -- {parti_ %s of %s}", booti_, n_boot+1, parti_, n_partition_actual))
      
      # Run single analysis
      rungood <- FALSE; runcounter <- 0; last_run_error <- NULL; while(!rungood){
        runcounter <- runcounter + 1 
        LatentRunResults_ <- try(lpmec_onerun(
          Y[boot_indices],
          observables[boot_indices,], 
          observables_groupings = observables_groupings,
          make_observables_groupings = make_observables_groupings, 
          estimation_method = estimation_method, 
          latent_estimation_fn = latent_estimation_fn, 
          ordinal = ordinal, 
          mcmc_control = mcmc_control, 
          conda_env = conda_env,
          conda_env_required = conda_env_required,
          partition = current_partition,
          partition_id = current_partition_id
        ),T) 
        if(!"try-error" %in% class(LatentRunResults_)){
          rungood <- TRUE
        } else {
          last_condition <- attr(LatentRunResults_, "condition")
          last_run_error <- if (!is.null(last_condition)) {
            conditionMessage(last_condition)
          } else {
            as.character(LatentRunResults_)
          }
          if(runcounter >= 100){
            stop("100 partition attempts failed... check data. Last error: ",
                 last_run_error, call. = FALSE)
          }
        }
      }
      
      
      # Tag each result with partition / bootstrap indices
      LatentRunResults_$PartitionIndex <- parti_
      LatentRunResults_$BootIndex      <- booti_
      LatentRunResults_$BootSampleSize <- current_m
      LatentRunResults_$BootstrapMethod <- bootstrap_method
      LatentRunResults_$BootstrapReplace <- boot_replace
      
      # If first iteration, initialize the main results object
      if(booti_ == 1L && parti_ == 1L){
        LatentRunResults <- LatentRunResults_
      } else {
        # Otherwise, cbind new columns onto existing results
        for(name_ in names(LatentRunResults_)){
          LatentRunResults[[name_]] <- .lpmec_cbind_intermediary(
            LatentRunResults[[name_]],
            LatentRunResults_[[name_]]
          )
        }
      }
    }
  }
  
  # Now prepend "Intermediary_" to each piece of stored output
  names(LatentRunResults) <- paste0("Intermediary_", names(LatentRunResults))
  
  boot_index_vec <- c(LatentRunResults$Intermediary_BootIndex)
  split_correlation_vec <- as.numeric(c(LatentRunResults$Intermediary_split_correlation))
  partition_valid <- is.finite(split_correlation_vec) & split_correlation_vec > 0
  invalid_reason <- ifelse(
    partition_valid,
    NA_character_,
    ifelse(is.finite(split_correlation_vec),
           "nonpositive_split_correlation",
           "nonfinite_split_correlation")
  )
  LatentRunResults$Intermediary_PartitionValid <- partition_valid
  LatentRunResults$Intermediary_PartitionInvalidReason <- invalid_reason

  boot_levels <- sort(unique(as.integer(boot_index_vec)))
  valid_partitions_by_boot <- vapply(boot_levels, function(boot_) {
    sum(partition_valid[boot_index_vec == boot_], na.rm = TRUE)
  }, integer(1L))
  invalid_partitions_by_boot <- vapply(boot_levels, function(boot_) {
    sum(!partition_valid[boot_index_vec == boot_], na.rm = TRUE)
  }, integer(1L))
  names(valid_partitions_by_boot) <- names(invalid_partitions_by_boot) <- as.character(boot_levels)
  original_valid_partitions <- valid_partitions_by_boot[["1"]]
  median_aggregation <- is.character(partition_aggregation) &&
    length(partition_aggregation) == 1L &&
    identical(partition_aggregation, "median")
  if (median_aggregation && is.finite(original_valid_partitions) && original_valid_partitions < 3L) {
    warning(
      "Median aggregation in the original sample is based on fewer than 3 valid partitions.",
      call. = FALSE
    )
  }
  if (n_boot >= 1L && is.finite(original_valid_partitions) && original_valid_partitions > 0L) {
    low_valid <- valid_partitions_by_boot[-1L] < max(3L, ceiling(original_valid_partitions / 2))
    if (any(low_valid, na.rm = TRUE)) {
      warning(
        "At least one bootstrap aggregate is based on fewer than half the original valid partitions or fewer than 3 valid partitions.",
        call. = FALSE
      )
    }
  }

  first_intermediary_column <- function(x) {
    if (is.null(dim(x))) {
      return(c(x))
    }
    c(x[, 1L])
  }

  aggregate_field <- function(field_name) {
    .lpmec_aggregate_numeric_by_boot(
      values = c(LatentRunResults[[paste0("Intermediary_", field_name)]]),
      boot_index = boot_index_vec,
      aggregation_fn = theSumFxn,
      valid = partition_valid
    )
  }
  summarize_field <- function(field_name) {
    theta_by_boot <- aggregate_field(field_name)
    theta0 <- unname(theta_by_boot[["1"]])
    theta_boot <- unname(theta_by_boot[names(theta_by_boot) != "1"])
    summary_boot_ci_type <- if (boot_ci_type == "rbc") "root" else boot_ci_type
    summary <- .lpmec_summarize_resampling(
      theta0 = theta0,
      theta_boot = theta_boot,
      n = length(Y),
      m = boot_m_resolved,
      bootstrap_method = bootstrap_method,
      boot_ci_type = summary_boot_ci_type,
      alpha = boot_alpha,
      rate = boot_rate,
      tau = boot_tau
    )
    summary$theta_by_boot <- theta_by_boot
    summary
  }
  estimate_field <- function(field_name) {
    unname(aggregate_field(field_name)[["1"]])
  }
  tstat_from_summary <- function(summary) {
    if (!is.finite(summary$se) || summary$se == 0) {
      return(NA_real_)
    }
    summary$estimate / summary$se
  }
  aggregate_parametric_se <- function(field_name) {
    values <- as.numeric(c(LatentRunResults[[paste0("Intermediary_", field_name)]]))
    values[!partition_valid] <- NA_real_
    out <- vapply(boot_levels, function(boot_) {
      x <- values[boot_index_vec == boot_]
      x <- x[is.finite(x)]
      if (length(x) < 1L) {
        return(NA_real_)
      }
      sqrt(sum(x^2)) / length(x)
    }, numeric(1L))
    names(out) <- as.character(boot_levels)
    unname(out[["1"]])
  }

  summary_field_names <- c(
    "ols_coef",
    "corrected_ols_coef",
    "corrected_ols_coef_alt",
    "iv_coef",
    "corrected_iv_coef",
    "bayesian_ols_coef_outer_normed",
    "bayesian_ols_coef_inner_normed",
    "m_stage_1_erv",
    "m_reduced_erv",
    "var_est_split"
  )

  make_bootstrap_aggregates <- function(summaries) {
    out <- data.frame(
      BootIndex = boot_levels,
      BootSampleSize = ifelse(boot_levels == 1L, length(Y), boot_m_resolved),
      ValidPartitions = as.integer(valid_partitions_by_boot[as.character(boot_levels)]),
      InvalidPartitions = as.integer(invalid_partitions_by_boot[as.character(boot_levels)])
    )
    for (field_name in summary_field_names) {
      out[[field_name]] <-
        unname(summaries[[field_name]]$theta_by_boot[as.character(boot_levels)])
    }
    out
  }

  run_rbc_pilots <- function() {
    if (!isTRUE(boot_rbc_spec$enabled) || length(boot_rbc_plan) < 1L) {
      return(data.frame())
    }

    rows <- list()
    errors <- character(0L)
    for (boot_ in boot_levels) {
      boot_indices <- boot_indices_by_boot[[as.character(boot_)]]
      for (plan_index in seq_along(boot_rbc_plan)) {
        plan_entry <- boot_rbc_plan[[plan_index]]
        subset_cols <- as.character(observables_groupings) %in% plan_entry$groups
        subset_observables <- observables[boot_indices, subset_cols, drop = FALSE]
        subset_groupings <- observables_groupings[subset_cols]
        partition_values <- vector("list", length(plan_entry$partitions))
        partition_valid_pilot <- rep(FALSE, length(plan_entry$partitions))

        for (partition_index in seq_along(plan_entry$partitions)) {
          current_partition <- plan_entry$partitions[[partition_index]]
          pilot_fit <- try(lpmec_onerun(
            Y[boot_indices],
            subset_observables,
            observables_groupings = subset_groupings,
            make_observables_groupings = make_observables_groupings,
            estimation_method = estimation_method,
            latent_estimation_fn = latent_estimation_fn,
            ordinal = ordinal,
            mcmc_control = mcmc_control,
            conda_env = conda_env,
            conda_env_required = conda_env_required,
            partition = current_partition,
            partition_id = current_partition$partition_id
          ), silent = TRUE)

          if (inherits(pilot_fit, "try-error")) {
            errors <- c(
              errors,
              paste0(
                "boot ", boot_,
                ", item_count ", plan_entry$item_count,
                ", subset ", plan_entry$subset_index,
                ", partition ", partition_index,
                ": ", as.character(pilot_fit)[1L]
              )
            )
            next
          }
          partition_values[[partition_index]] <- pilot_fit
          split_correlation <- as.numeric(pilot_fit$split_correlation)[1L]
          partition_valid_pilot[[partition_index]] <-
            is.finite(split_correlation) && split_correlation > 0
        }

        row <- data.frame(
          BootIndex = as.integer(boot_),
          ItemCount = as.integer(plan_entry$item_count),
          SubsetIndex = as.integer(plan_entry$subset_index),
          ValidPartitions = sum(partition_valid_pilot, na.rm = TRUE),
          InvalidPartitions = sum(!partition_valid_pilot, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
        for (field_name in summary_field_names) {
          values <- vapply(partition_values, function(pilot_fit) {
            if (is.null(pilot_fit) || !field_name %in% names(pilot_fit)) {
              return(NA_real_)
            }
            as.numeric(pilot_fit[[field_name]])[1L]
          }, numeric(1L))
          values[!partition_valid_pilot] <- NA_real_
          finite_values <- values[is.finite(values)]
          row[[field_name]] <- if (length(finite_values) < 1L) {
            NA_real_
          } else {
            as.numeric(theSumFxn(finite_values))
          }
        }
        rows[[length(rows) + 1L]] <- row
      }
    }

    out <- if (length(rows) < 1L) {
      data.frame()
    } else {
      do.call(rbind, rows)
    }
    attr(out, "errors") <- errors
    out
  }

  field_summaries <- stats::setNames(
    lapply(summary_field_names, summarize_field),
    summary_field_names
  )

  boot_calibration_coverage <- NULL
  boot_calibration_diagnostics <- NULL
  boot_rbc_diagnostics <- NULL
  boot_rbc_pilot_aggregates <- NULL
  boot_rbc_raw_aggregates <- NULL
  boot_rbc_errors <- NULL
  if (boot_ci_type == "rbc") {
    boot_rbc_raw_aggregates <- make_bootstrap_aggregates(field_summaries)
    boot_rbc_pilot_aggregates <- run_rbc_pilots()
    boot_rbc_errors <- attr(boot_rbc_pilot_aggregates, "errors")
    rbc_result <- .lpmec_apply_rbc_summaries(
      field_summaries = field_summaries,
      pilot_aggregates = boot_rbc_pilot_aggregates,
      summary_field_names = summary_field_names,
      boot_levels = boot_levels,
      full_count = boot_rbc_spec$full_count,
      item_counts = boot_rbc_spec$item_counts,
      aggregation_fn = theSumFxn,
      n = length(Y),
      m = boot_m_resolved,
      bootstrap_method = bootstrap_method,
      alpha = boot_alpha,
      rate = boot_rate,
      tau = boot_tau
    )
    field_summaries <- rbc_result$field_summaries
    boot_rbc_diagnostics <- rbc_result$diagnostics
    finite_fallback <- boot_rbc_diagnostics$status != "rbc" &
      is.finite(boot_rbc_diagnostics$original_theta_raw)
    if (any(finite_fallback, na.rm = TRUE)) {
      warning(
        "RBC was unavailable for ",
        paste(boot_rbc_diagnostics$field[finite_fallback], collapse = ", "),
        "; using uncorrected root summaries for those fields.",
        call. = FALSE
      )
    }
  }
  if (boot_ci_type == "root_calibrated") {
    if (boot_m_resolved < 3L) {
      stop("boot_ci_type = \"root_calibrated\" requires resolved boot_m >= 3.")
    }

    calibration_result <- .lpmec_with_local_seed(boot_calibration_seed, {
      inner_m_spec <- .lpmec_resolve_m(
        n = boot_m_resolved,
        boot_m = NULL,
        boot_m_rule = "power",
        boot_m_exponent = boot_m_exponent,
        boot_m_grid = NULL,
        min_m = min(10L, boot_m_resolved - 1L),
        bootstrap_method = bootstrap_method
      )
      inner_m <- inner_m_spec$m
      rate_m_calibration <- .lpmec_rate_value(
        boot_m_resolved,
        rate = boot_rate,
        tau = boot_tau,
        label = "calibration subsample size"
      )

      calibration_checks <- vector("list", boot_calibration_n)
      calibration_errors <- character(0L)
      full_estimates <- vapply(field_summaries, function(summary) {
        as.numeric(summary$estimate)[1L]
      }, numeric(1L))

      for (cal_i in seq_len(boot_calibration_n)) {
        cal_indices <- .lpmec_resample_indices(
          n = length(Y),
          m = boot_m_resolved,
          replace = boot_replace,
          boot_basis = if (boot_basis_supplied) boot_basis else NULL
        )

        inner_fit <- try(
          suppressMessages(lpmec(
            Y = Y[cal_indices],
            observables = observables[cal_indices, , drop = FALSE],
            observables_groupings = observables_groupings,
            orientation_signs = NULL,
            make_observables_groupings = make_observables_groupings,
            n_boot = boot_calibration_inner_n_boot,
            n_partition = n_partition_actual,
            partition_aggregation = partition_aggregation,
            partition_aggregation_probs = partition_aggregation_probs,
            bootstrap_method = bootstrap_method,
            boot_m = inner_m,
            boot_m_rule = "fixed",
            boot_m_exponent = boot_m_exponent,
            boot_m_grid = NULL,
            boot_m_replace = boot_replace,
            boot_ci_type = "root",
            boot_alpha = boot_alpha,
            boot_rate = boot_rate,
            boot_tau = boot_tau,
            partition_set = if (fix_partitions) partition_list else NULL,
            fix_partitions = fix_partitions,
            seed = NULL,
            return_intermediaries = FALSE,
            ordinal = ordinal,
            estimation_method = estimation_method,
            latent_estimation_fn = latent_estimation_fn,
            mcmc_control = mcmc_control,
            conda_env = conda_env,
            conda_env_required = conda_env_required
          )),
          silent = TRUE
        )

        if (inherits(inner_fit, "try-error")) {
          calibration_errors <- c(
            calibration_errors,
            paste0("calibration subsample ", cal_i, ": ", as.character(inner_fit)[1L])
          )
          next
        }

        field_rows <- lapply(summary_field_names, function(field_name) {
          theta_full <- full_estimates[[field_name]]
          theta_m <- as.numeric(inner_fit[[field_name]])[1L]
          root_draws_inner <- inner_fit$root_draws[[field_name]]
          root_draws_inner <- as.numeric(root_draws_inner[is.finite(root_draws_inner)])
          if (!is.finite(theta_full) || !is.finite(theta_m) || length(root_draws_inner) < 2L) {
            return(NULL)
          }

          do.call(rbind, lapply(boot_calibration_cut_grid, function(cut) {
            lower <- theta_m -
              stats::quantile(root_draws_inner, 1 - cut, na.rm = TRUE, names = FALSE) /
              rate_m_calibration
            upper <- theta_m -
              stats::quantile(root_draws_inner, cut, na.rm = TRUE, names = FALSE) /
              rate_m_calibration
            lower_covered <- theta_full >= lower
            upper_covered <- theta_full <= upper
            data.frame(
              calibration_index = cal_i,
              field = field_name,
              cut = as.numeric(cut),
              lower_covered = isTRUE(lower_covered),
              upper_covered = isTRUE(upper_covered),
              twosided_covered = isTRUE(lower_covered) && isTRUE(upper_covered),
              stringsAsFactors = FALSE
            )
          }))
        })
        calibration_checks[[cal_i]] <- do.call(rbind, field_rows)
      }

      calibration_checks <- do.call(rbind, calibration_checks)
      if (is.null(calibration_checks) || nrow(calibration_checks) < 1L) {
        warning(
          "Root calibration produced no valid calibration intervals; returning uncalibrated root intervals.",
          call. = FALSE
        )
        diagnostics <- data.frame(
          field = summary_field_names,
          root_left_tail = boot_alpha / 2,
          root_right_tail = boot_alpha / 2,
          selected_lower_coverage = NA_real_,
          selected_upper_coverage = NA_real_,
          selected_twosided_coverage = NA_real_,
          valid_calibration_subsamples = 0L,
          target_lower_coverage = 1 - boot_alpha / 2,
          target_upper_coverage = 1 - boot_alpha / 2,
          target_twosided_coverage = 1 - boot_alpha,
          calibration_tail = boot_calibration_tail,
          inner_m = inner_m,
          calibration_n = boot_calibration_n,
          inner_n_boot = boot_calibration_inner_n_boot,
          calibration_status = "no_valid_intervals",
          stringsAsFactors = FALSE
        )
        return(list(
          field_summaries = field_summaries,
          coverage = data.frame(),
          diagnostics = diagnostics,
          inner_m = inner_m,
          errors = calibration_errors
        ))
      }

      coverage_keys <- unique(calibration_checks[, c("field", "cut"), drop = FALSE])
      coverage <- do.call(rbind, lapply(seq_len(nrow(coverage_keys)), function(i) {
        field_name <- coverage_keys$field[[i]]
        cut <- coverage_keys$cut[[i]]
        x <- calibration_checks[
          calibration_checks$field == field_name & calibration_checks$cut == cut,
          ,
          drop = FALSE
        ]
        data.frame(
          field = field_name,
          cut = cut,
          lower_coverage = mean(x$lower_covered),
          upper_coverage = mean(x$upper_covered),
          twosided_coverage = mean(x$twosided_covered),
          n_calibration = nrow(x),
          stringsAsFactors = FALSE
        )
      }))

      selected <- .lpmec_select_root_calibration_cutoffs(
        calibration_coverage = coverage,
        alpha = boot_alpha,
        tail = boot_calibration_tail
      )

      updated_summaries <- field_summaries
      diagnostics <- vector("list", length(summary_field_names))
      names(diagnostics) <- summary_field_names
      selected_pair_coverage <- function(field_name, root_left_tail, root_right_tail) {
        lower_rows <- calibration_checks[
          calibration_checks$field == field_name &
            abs(calibration_checks$cut - root_right_tail) < sqrt(.Machine$double.eps),
          c("calibration_index", "lower_covered"),
          drop = FALSE
        ]
        upper_rows <- calibration_checks[
          calibration_checks$field == field_name &
            abs(calibration_checks$cut - root_left_tail) < sqrt(.Machine$double.eps),
          c("calibration_index", "upper_covered"),
          drop = FALSE
        ]
        names(lower_rows)[names(lower_rows) == "lower_covered"] <- "selected_lower_covered"
        names(upper_rows)[names(upper_rows) == "upper_covered"] <- "selected_upper_covered"
        merged <- merge(lower_rows, upper_rows, by = "calibration_index")
        if (nrow(merged) < 1L) {
          return(NA_real_)
        }
        mean(merged$selected_lower_covered & merged$selected_upper_covered)
      }
      for (field_name in summary_field_names) {
        selected_row <- selected[selected$field == field_name, , drop = FALSE]
        if (nrow(selected_row) < 1L) {
          diagnostics[[field_name]] <- data.frame(
            field = field_name,
            root_left_tail = boot_alpha / 2,
            root_right_tail = boot_alpha / 2,
            selected_lower_coverage = NA_real_,
            selected_upper_coverage = NA_real_,
            selected_twosided_coverage = NA_real_,
            valid_calibration_subsamples = 0L,
            calibration_status = "not_calibrated",
            stringsAsFactors = FALSE
          )
          next
        }

        updated_summaries[[field_name]] <- .lpmec_apply_root_tail_cutoffs(
          summary = updated_summaries[[field_name]],
          n = length(Y),
          rate = boot_rate,
          tau = boot_tau,
          root_left_tail = selected_row$root_left_tail[[1L]],
          root_right_tail = selected_row$root_right_tail[[1L]]
        )
        field_coverage <- coverage[coverage$field == field_name, , drop = FALSE]
        diagnostics[[field_name]] <- data.frame(
          field = field_name,
          root_left_tail = selected_row$root_left_tail[[1L]],
          root_right_tail = selected_row$root_right_tail[[1L]],
          selected_lower_coverage = selected_row$selected_lower_coverage[[1L]],
          selected_upper_coverage = selected_row$selected_upper_coverage[[1L]],
          selected_twosided_coverage = selected_pair_coverage(
            field_name,
            selected_row$root_left_tail[[1L]],
            selected_row$root_right_tail[[1L]]
          ),
          valid_calibration_subsamples = max(field_coverage$n_calibration, na.rm = TRUE),
          calibration_status = "calibrated",
          stringsAsFactors = FALSE
        )
      }
      diagnostics <- do.call(rbind, diagnostics)
      diagnostics$target_lower_coverage <- 1 - boot_alpha / 2
      diagnostics$target_upper_coverage <- 1 - boot_alpha / 2
      diagnostics$target_twosided_coverage <- 1 - boot_alpha
      diagnostics$calibration_tail <- boot_calibration_tail
      diagnostics$inner_m <- inner_m
      diagnostics$calibration_n <- boot_calibration_n
      diagnostics$inner_n_boot <- boot_calibration_inner_n_boot
      diagnostics$calibration_errors <- if (length(calibration_errors) > 0L) {
        paste(calibration_errors, collapse = "\n")
      } else {
        NA_character_
      }

      list(
        field_summaries = updated_summaries,
        coverage = coverage,
        diagnostics = diagnostics,
        inner_m = inner_m,
        errors = calibration_errors
      )
    })

    field_summaries <- calibration_result$field_summaries
    boot_calibration_coverage <- calibration_result$coverage
    boot_calibration_diagnostics <- calibration_result$diagnostics
  }

  bootstrap_aggregates <- make_bootstrap_aggregates(field_summaries)
  root_draws <- lapply(field_summaries, function(summary) summary$root_draws)
  bootstrap_failure_diagnostics <- data.frame(
    field = summary_field_names,
    n_draws = vapply(field_summaries, function(summary) summary$n_draws, integer(1L)),
    n_failed = vapply(field_summaries, function(summary) summary$n_failed, integer(1L)),
    success_rate = vapply(field_summaries, function(summary) summary$success_rate, numeric(1L)),
    row.names = NULL
  )
  bootstrap_success_rate <- field_summaries$corrected_iv_coef$success_rate
  if (n_boot >= 1L &&
      is.finite(bootstrap_success_rate) &&
      bootstrap_success_rate < 0.90) {
    warning(
      "Corrected-IV bootstrap success rate is below 0.90; inspect bootstrap_failure_diagnostics.",
      call. = FALSE
    )
  }

  summary_result_fields <- function(specs) {
    out <- list()
    for (spec in specs) {
      summary <- spec$summary
      out[[spec$coef]] <- summary$estimate
      if (!is.null(spec$parametric_se)) {
        out[[spec$parametric_se]] <- aggregate_parametric_se(spec$parametric_source)
      }
      out[[spec$se]] <- summary$se
      out[[spec$lower]] <- summary$lower
      out[[spec$upper]] <- summary$upper
      out[[spec$tstat]] <- tstat_from_summary(summary)
    }
    out
  }

  core_results <- c(
    summary_result_fields(list(
      list(
        summary = field_summaries$ols_coef,
        coef = "ols_coef",
        se = "ols_se",
        lower = "ols_lower",
        upper = "ols_upper",
        tstat = "ols_tstat"
      )
    )),
    list(
      "corrected_ols_coef_a" = estimate_field("corrected_ols_coef_a"),
      "corrected_ols_coef_b" = estimate_field("corrected_ols_coef_b")
    ),
    summary_result_fields(list(
      list(
        summary = field_summaries$corrected_ols_coef,
        coef = "corrected_ols_coef",
        se = "corrected_ols_se",
        lower = "corrected_ols_lower",
        upper = "corrected_ols_upper",
        tstat = "corrected_ols_tstat"
      ),
      list(
        summary = field_summaries$corrected_ols_coef_alt,
        coef = "corrected_ols_coef_alt",
        se = "corrected_ols_se_alt",
        lower = "corrected_ols_lower_alt",
        upper = "corrected_ols_upper_alt",
        tstat = "corrected_ols_tstat_alt"
      )
    )),
    list(
      "iv_coef_a" = estimate_field("iv_coef_a"),
      "iv_coef_b" = estimate_field("iv_coef_b")
    ),
    summary_result_fields(list(
      list(
        summary = field_summaries$iv_coef,
        coef = "iv_coef",
        se = "iv_se",
        lower = "iv_lower",
        upper = "iv_upper",
        tstat = "iv_tstat"
      )
    )),
    list(
      "corrected_iv_coef_a" = estimate_field("corrected_iv_coef_a"),
      "corrected_iv_coef_b" = estimate_field("corrected_iv_coef_b")
    ),
    summary_result_fields(list(
      list(
        summary = field_summaries$corrected_iv_coef,
        coef = "corrected_iv_coef",
        se = "corrected_iv_se",
        lower = "corrected_iv_lower",
        upper = "corrected_iv_upper",
        tstat = "corrected_iv_tstat"
      ),
      list(
        summary = field_summaries$bayesian_ols_coef_outer_normed,
        coef = "bayesian_ols_coef_outer_normed",
        parametric_se = "bayesian_ols_se_outer_normed_parametric",
        parametric_source = "bayesian_ols_se_outer_normed",
        se = "bayesian_ols_se_outer_normed",
        lower = "bayesian_ols_lower_outer_normed",
        upper = "bayesian_ols_upper_outer_normed",
        tstat = "bayesian_ols_tstat_outer_normed"
      ),
      list(
        summary = field_summaries$bayesian_ols_coef_inner_normed,
        coef = "bayesian_ols_coef_inner_normed",
        parametric_se = "bayesian_ols_se_inner_normed_parametric",
        parametric_source = "bayesian_ols_se_inner_normed",
        se = "bayesian_ols_se_inner_normed",
        lower = "bayesian_ols_lower_inner_normed",
        upper = "bayesian_ols_upper_inner_normed",
        tstat = "bayesian_ols_tstat_inner_normed"
      ),
      list(
        summary = field_summaries$m_stage_1_erv,
        coef = "m_stage_1_erv",
        se = "m_stage_1_erv_se",
        lower = "m_stage_1_erv_lower",
        upper = "m_stage_1_erv_upper",
        tstat = "m_stage_1_erv_tstat"
      ),
      list(
        summary = field_summaries$m_reduced_erv,
        coef = "m_reduced_erv",
        se = "m_reduced_erv_se",
        lower = "m_reduced_erv_lower",
        upper = "m_reduced_erv_upper",
        tstat = "m_reduced_erv_tstat"
      )
    ))
  )
  var_est_split_summary <- field_summaries$var_est_split

  results <- c(
    core_results,
    list(
      "mcmc_joint2_ability_mean_ess_pct" = estimate_field("mcmc_joint2_ability_mean_ess_pct"),
      "mcmc_joint2_ability_min_ess_pct" = estimate_field("mcmc_joint2_ability_min_ess_pct"),
      "mcmc_joint2_max_rhat" = estimate_field("mcmc_joint2_max_rhat"),
      "mcmc_joint2_num_divergent" = estimate_field("mcmc_joint2_num_divergent"),
      "mcmc_joint2_mean_accept_prob" = estimate_field("mcmc_joint2_mean_accept_prob"),
      "mcmc_joint2_orientation_n_flipped" = estimate_field("mcmc_joint2_orientation_n_flipped"),
      "mcmc_joint2_orientation_prop_flipped" = estimate_field("mcmc_joint2_orientation_prop_flipped"),
      "mcmc_joint2_orientation_min_abs_cor" = estimate_field("mcmc_joint2_orientation_min_abs_cor"),

      "x_est1" = first_intermediary_column(LatentRunResults$Intermediary_x_est1),
      "x_est2" = first_intermediary_column(LatentRunResults$Intermediary_x_est2),
      "var_est_split"    = var_est_split_summary$estimate,
      "var_est_split_se" = var_est_split_summary$se,

      "bootstrap_method" = bootstrap_method,
      "boot_m" = boot_m_resolved,
      "boot_m_ratio" = boot_m_resolved / length(Y),
      "boot_replace" = boot_replace,
      "boot_ci_type" = boot_ci_type,
      "boot_rate" = boot_rate,
      "boot_calibration" = if (boot_ci_type == "root_calibrated") {
        "nested_subsampling"
      } else {
        "none"
      },
      "boot_calibration_tail" = boot_calibration_tail,
      "boot_calibration_n" = if (boot_ci_type == "root_calibrated") boot_calibration_n else NA_integer_,
      "boot_calibration_inner_n_boot" = if (boot_ci_type == "root_calibrated") {
        boot_calibration_inner_n_boot
      } else {
        NA_integer_
      },
      "boot_calibration_cut_grid" = if (boot_ci_type == "root_calibrated") {
        boot_calibration_cut_grid
      } else {
        numeric(0L)
      },
      "boot_calibration_coverage" = boot_calibration_coverage,
      "boot_calibration_diagnostics" = boot_calibration_diagnostics,
      "boot_rbc_item_counts" = if (boot_ci_type == "rbc") boot_rbc_spec$item_counts else integer(0L),
      "boot_rbc_n_subsets" = if (boot_ci_type == "rbc") boot_rbc_spec$n_subsets else NA_integer_,
      "boot_rbc_diagnostics" = boot_rbc_diagnostics,
      "boot_rbc_raw_aggregates" = boot_rbc_raw_aggregates,
      "boot_rbc_pilot_aggregates" = boot_rbc_pilot_aggregates,
      "boot_rbc_errors" = if (boot_ci_type == "rbc") boot_rbc_errors else character(0L),
      "partition_set" = if (fix_partitions) partition_list else NULL,
      "fix_partitions" = fix_partitions,
      "n_unique_partitions" = n_partition_actual,
      "valid_partitions_by_boot" = valid_partitions_by_boot,
      "invalid_partitions_by_boot" = invalid_partitions_by_boot,
      "bootstrap_success_rate" = bootstrap_success_rate,
      "m_selection" = m_spec,
      "root_draws" = root_draws,
      "bootstrap_aggregates" = bootstrap_aggregates,
      "bootstrap_failure_diagnostics" = bootstrap_failure_diagnostics
    ))
  if (return_intermediaries) {
    results <- c(results, LatentRunResults)
  }
  class(results) <- "lpmec"
  return(results)
  
}
