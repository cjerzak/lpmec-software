.lpmec_with_local_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(force(expr))
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("'seed' must be NULL or a single finite numeric value.")
  }

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
  force(expr)
}

.lpmec_partition_key <- function(split1_names, split2_names) {
  side1 <- paste(sort(as.character(split1_names)), collapse = "\r")
  side2 <- paste(sort(as.character(split2_names)), collapse = "\r")
  paste(sort(c(side1, side2)), collapse = "\f")
}

.lpmec_validate_partition <- function(partition,
                                      groups,
                                      partition_index = NULL,
                                      require_balanced = TRUE) {
  label <- if (is.null(partition_index)) {
    "'partition'"
  } else {
    paste0("'partition_set[[", partition_index, "]]'")
  }
  if (!is.list(partition)) {
    stop(label, " must be a list.")
  }
  if (is.null(partition$split1_names) || is.null(partition$split2_names)) {
    stop(label, " must contain 'split1_names' and 'split2_names'.")
  }

  split1_names <- as.character(partition$split1_names)
  split2_names <- as.character(partition$split2_names)
  if (length(split1_names) < 1L || length(split2_names) < 1L) {
    stop(label, " must contain non-empty split halves.")
  }
  if (anyDuplicated(split1_names) || anyDuplicated(split2_names)) {
    stop(label, " contains duplicate group names within a split.")
  }
  if (length(intersect(split1_names, split2_names)) > 0L) {
    stop(label, " assigns at least one group to both split halves.")
  }

  groups <- as.character(groups)
  combined <- c(split1_names, split2_names)
  if (!identical(sort(combined), sort(groups))) {
    missing_groups <- setdiff(groups, combined)
    extra_groups <- setdiff(combined, groups)
    details <- character(0L)
    if (length(missing_groups) > 0L) {
      details <- c(details, paste0("missing: ", paste(missing_groups, collapse = ", ")))
    }
    if (length(extra_groups) > 0L) {
      details <- c(details, paste0("unknown: ", paste(extra_groups, collapse = ", ")))
    }
    stop(label, " must assign every observable group exactly once",
         if (length(details) > 0L) paste0(" (", paste(details, collapse = "; "), ")") else "",
         ".")
  }

  if (require_balanced && abs(length(split1_names) - length(split2_names)) > 1L) {
    stop(label, " must be balanced: split sizes may differ by at most one group.")
  }

  list(
    partition_id = if (is.null(partition$partition_id)) {
      if (is.null(partition_index)) NA_character_ else as.character(partition_index)
    } else {
      as.character(partition$partition_id)
    },
    split1_names = split1_names,
    split2_names = split2_names
  )
}

.lpmec_make_partitions <- function(observables_groupings,
                                   n_partition,
                                   partition_set = NULL,
                                   seed = NULL) {
  if (!is.numeric(n_partition) ||
      length(n_partition) != 1L ||
      !is.finite(n_partition) ||
      n_partition != floor(n_partition) ||
      n_partition < 1L) {
    stop("'n_partition' must be a single positive integer.")
  }
  n_partition <- as.integer(n_partition)

  groups <- unique(as.character(observables_groupings))
  if (length(groups) < 2L) {
    stop("At least 2 unique observable groups are required for split-half partitions.")
  }

  if (!is.null(partition_set)) {
    if (!is.list(partition_set) || length(partition_set) < 1L) {
      stop("'partition_set' must be NULL or a non-empty list of partitions.")
    }
    out <- lapply(seq_along(partition_set), function(i) {
      .lpmec_validate_partition(partition_set[[i]], groups, i)
    })
    keys <- vapply(out, function(partition) {
      .lpmec_partition_key(partition$split1_names, partition$split2_names)
    }, character(1L))
    if (anyDuplicated(keys)) {
      stop("'partition_set' contains duplicate partitions up to swapping split halves.")
    }
    return(out)
  }

  .lpmec_with_local_seed(seed, {
    split_size <- floor(length(groups) / 2L)
    possible_count <- choose(length(groups), split_size)
    if (length(groups) %% 2L == 0L) {
      possible_count <- possible_count / 2
    }

    make_partition <- function(split1_names, id) {
      split1_names <- as.character(split1_names)
      split2_names <- groups[!groups %in% split1_names]
      list(
        partition_id = as.character(id),
        split1_names = split1_names,
        split2_names = split2_names
      )
    }

    if (is.finite(possible_count) &&
        possible_count <= max(n_partition * 20L, 5000L)) {
      combos <- utils::combn(groups, split_size, simplify = FALSE)
      keys <- vapply(combos, function(split1_names) {
        .lpmec_partition_key(split1_names, groups[!groups %in% split1_names])
      }, character(1L))
      combos <- combos[!duplicated(keys)]
      keys <- keys[!duplicated(keys)]
      if (length(combos) < n_partition) {
        warning(
          "Only ", length(combos),
          " unique balanced partitions are possible; using all of them.",
          call. = FALSE
        )
      }
      selected <- if (length(combos) <= n_partition) {
        seq_along(combos)
      } else {
        sample(seq_along(combos), n_partition, replace = FALSE)
      }
      out <- lapply(seq_along(selected), function(i) {
        make_partition(combos[[selected[[i]]]], i)
      })
      return(out)
    }

    out <- list()
    seen <- character(0L)
    max_attempts <- max(1000L, n_partition * 1000L)
    attempts <- 0L
    while (length(out) < n_partition && attempts < max_attempts) {
      attempts <- attempts + 1L
      split1_names <- sample(groups, split_size, replace = FALSE)
      split2_names <- groups[!groups %in% split1_names]
      key <- .lpmec_partition_key(split1_names, split2_names)
      if (!key %in% seen) {
        seen <- c(seen, key)
        out[[length(out) + 1L]] <- make_partition(split1_names, length(out) + 1L)
      }
    }
    if (length(out) < n_partition) {
      warning(
        "Only generated ", length(out), " unique balanced partitions after ",
        attempts, " attempts; using the generated set.",
        call. = FALSE
      )
    }
    out
  })
}

.lpmec_resolve_bootstrap_method <- function(bootstrap_method,
                                            partition_aggregation,
                                            boot_ci_type,
                                            warn_nonsmooth = TRUE) {
  bootstrap_method <- match.arg(
    bootstrap_method,
    c("n_out_of_n", "m_out_of_n", "subsampling", "auto")
  )
  boot_ci_type <- match.arg(boot_ci_type, c("auto", "root", "root_calibrated", "percentile"))

  median_aggregation <- is.character(partition_aggregation) &&
    length(partition_aggregation) == 1L &&
    identical(partition_aggregation, "median")

  if (bootstrap_method == "auto") {
    bootstrap_method <- if (median_aggregation) "subsampling" else "n_out_of_n"
  }

  if (boot_ci_type == "auto") {
    boot_ci_type <- if (bootstrap_method == "n_out_of_n") "percentile" else "root"
  }

  if (isTRUE(warn_nonsmooth) && median_aggregation && bootstrap_method == "n_out_of_n") {
    warning(
      "partition_aggregation = \"median\" is nonsmooth. The ordinary n-out-of-n ",
      "bootstrap is retained for backward compatibility and practical approximation. ",
      "For the formal nonsmooth-functional route, use bootstrap_method = ",
      "\"subsampling\" or \"m_out_of_n\" with boot_ci_type = \"root\" ",
      "or \"root_calibrated\".",
      call. = FALSE
    )
  }

  if (bootstrap_method != "n_out_of_n" && boot_ci_type == "percentile") {
    warning(
      "Raw percentile intervals for m < n resampling are not root-scaled. ",
      "For the formal nonsmooth-functional route, use boot_ci_type = \"root\" ",
      "or \"root_calibrated\".",
      call. = FALSE
    )
  }

  list(
    bootstrap_method = bootstrap_method,
    boot_ci_type = boot_ci_type
  )
}

.lpmec_resolve_m <- function(n,
                             boot_m = NULL,
                             boot_m_rule = "power",
                             boot_m_exponent = 0.70,
                             boot_m_grid = NULL,
                             min_m = 10L,
                             bootstrap_method = "subsampling") {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n != floor(n) || n < 2L) {
    stop("'n' must be a single integer greater than or equal to 2.")
  }
  n <- as.integer(n)
  bootstrap_method <- match.arg(bootstrap_method, c("n_out_of_n", "m_out_of_n", "subsampling"))
  boot_m_rule <- match.arg(boot_m_rule, c("power", "fixed", "grid_stability"))
  if (!is.numeric(min_m) ||
      length(min_m) != 1L ||
      !is.finite(min_m) ||
      min_m != floor(min_m) ||
      min_m < 2L) {
    stop("'min_m' must be a single integer greater than or equal to 2.")
  }
  min_m <- as.integer(min_m)

  warnings <- character(0L)
  if (bootstrap_method == "n_out_of_n") {
    if (!is.null(boot_m)) {
      warnings <- c(warnings, "'boot_m' is ignored when bootstrap_method = \"n_out_of_n\".")
      warning(warnings[length(warnings)], call. = FALSE)
    }
    return(list(
      m = n,
      m_rule = "n_out_of_n",
      m_grid = integer(0L),
      warnings = warnings
    ))
  }

  explicit_m <- !is.null(boot_m)
  if (explicit_m) {
    if (!is.numeric(boot_m) ||
        length(boot_m) != 1L ||
        !is.finite(boot_m) ||
        boot_m != floor(boot_m)) {
      stop("'boot_m' must be NULL or a single integer.")
    }
    m <- as.integer(boot_m)
    if (m < 2L || m >= n) {
      stop("'boot_m' must satisfy 2 <= boot_m < n for m-out-of-n/subsampling.")
    }
    m_rule <- "fixed"
  } else if (boot_m_rule == "fixed") {
    stop("'boot_m' must be provided when boot_m_rule = \"fixed\".")
  } else if (boot_m_rule == "grid_stability") {
    if (is.null(boot_m_grid)) {
      boot_m_grid <- unique(floor(n ^ seq(0.55, 0.85, by = 0.05)))
    }
    boot_m_grid <- as.integer(unique(boot_m_grid[is.finite(boot_m_grid)]))
    boot_m_grid <- sort(boot_m_grid[boot_m_grid >= 2L & boot_m_grid < n])
    if (length(boot_m_grid) < 1L) {
      stop("'boot_m_grid' contains no valid values with 2 <= m < n.")
    }
    target <- floor(n ^ boot_m_exponent)
    m <- boot_m_grid[which.min(abs(boot_m_grid - target))]
    m_rule <- "grid_stability"
    warnings <- c(
      warnings,
      "boot_m_rule = \"grid_stability\" records the candidate grid but does not run a pilot selector yet; using the grid value closest to floor(n^boot_m_exponent)."
    )
    warning(warnings[length(warnings)], call. = FALSE)
  } else {
    if (!is.numeric(boot_m_exponent) ||
        length(boot_m_exponent) != 1L ||
        !is.finite(boot_m_exponent) ||
        boot_m_exponent <= 0) {
      stop("'boot_m_exponent' must be a single positive finite numeric value.")
    }
    m <- floor(n ^ boot_m_exponent)
    m_rule <- "power"
  }

  if (!explicit_m && m < min_m && n - 1L >= min_m) {
    m <- min_m
  }
  if (!explicit_m && m >= n) {
    m <- n - 1L
  }
  if (m < 2L) {
    m <- 2L
  }
  if (m >= n) {
    stop("Cannot resolve a valid m < n for m-out-of-n/subsampling with n = ", n, ".")
  }

  if (m < min_m) {
    warnings <- c(
      warnings,
      paste0(
        if (explicit_m) {
          "Explicit boot_m is"
        } else {
          paste0("n is too small to use min_m = ", min_m,
                 " while keeping m < n; resolved m is")
        },
        " ", m,
        "; inspect finite-sample sensitivity."
      )
    )
    warning(warnings[length(warnings)], call. = FALSE)
  }
  if (m / n > 0.50) {
    warnings <- c(
      warnings,
      paste0(
        "Resolved m/n = ", signif(m / n, 3),
        "; the asymptotic condition m/n -> 0 is poorly approximated. ",
        "Inspect m sensitivity or consider a smooth aggregation robustness check."
      )
    )
    warning(warnings[length(warnings)], call. = FALSE)
  }

  list(
    m = as.integer(m),
    m_rule = m_rule,
    m_grid = if (is.null(boot_m_grid)) integer(0L) else as.integer(boot_m_grid),
    warnings = warnings
  )
}

.lpmec_resample_indices <- function(n,
                                    m,
                                    replace,
                                    boot_basis = NULL) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n != floor(n) || n < 1L) {
    stop("'n' must be a single positive integer.")
  }
  if (!is.numeric(m) || length(m) != 1L || !is.finite(m) || m != floor(m) || m < 1L) {
    stop("'m' must be a single positive integer.")
  }
  if (!is.logical(replace) || length(replace) != 1L || is.na(replace)) {
    stop("'replace' must be a single TRUE/FALSE value.")
  }
  n <- as.integer(n)
  m <- as.integer(m)
  if (!replace && m > n) {
    stop("Cannot sample m > n without replacement.")
  }
  if (!is.null(boot_basis) && length(boot_basis) != n) {
    stop("'boot_basis' must have length n.")
  }
  if (is.null(boot_basis) || length(unique(boot_basis)) == n) {
    return(sample(seq_len(n), m, replace = replace))
  }

  strata <- split(seq_len(n), as.character(boot_basis))
  stratum_sizes <- lengths(strata)
  raw_alloc <- m * stratum_sizes / n
  alloc <- floor(raw_alloc)
  remainder <- m - sum(alloc)
  if (remainder > 0L) {
    fractional_order <- order(raw_alloc - alloc, stratum_sizes, decreasing = TRUE)
    alloc[fractional_order[seq_len(remainder)]] <-
      alloc[fractional_order[seq_len(remainder)]] + 1L
  }

  if (!replace && any(alloc > stratum_sizes)) {
    warning(
      "At least one stratum allocation exceeded the available rows for subsampling; ",
      "capping that allocation and redistributing remaining rows deterministically.",
      call. = FALSE
    )
    excess <- sum(pmax(alloc - stratum_sizes, 0L))
    alloc <- pmin(alloc, stratum_sizes)
    while (excess > 0L) {
      capacity <- stratum_sizes - alloc
      if (!any(capacity > 0L)) {
        stop("Cannot allocate stratified subsample of length m without replacement.")
      }
      j <- which.max(capacity)
      alloc[j] <- alloc[j] + 1L
      excess <- excess - 1L
    }
  }

  while (sum(alloc) > m) {
    j <- which.max(alloc)
    alloc[j] <- alloc[j] - 1L
  }
  while (sum(alloc) < m) {
    capacity <- if (replace) rep(Inf, length(alloc)) else stratum_sizes - alloc
    j <- which.max(capacity)
    if (!is.finite(capacity[j]) && replace) {
      alloc[j] <- alloc[j] + 1L
    } else if (capacity[j] > 0L) {
      alloc[j] <- alloc[j] + 1L
    } else {
      stop("Cannot allocate stratified resample to length m.")
    }
  }

  out <- unlist(
    Map(function(idx, size) {
      if (size < 1L) {
        return(integer(0L))
      }
      sample(idx, size, replace = replace)
    }, strata, alloc),
    use.names = FALSE
  )
  if (length(out) != m) {
    stop("Internal error: resample index length is ", length(out), ", expected ", m, ".")
  }
  out
}

.lpmec_rate_value <- function(k,
                              rate = "sqrt_n",
                              tau = NULL,
                              label = "k") {
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k <= 0) {
    stop("'", label, "' must be a single positive finite numeric value.")
  }
  rate <- match.arg(rate, c("sqrt_n", "custom"))
  if (rate == "sqrt_n") {
    return(sqrt(k))
  }
  if (!is.function(tau)) {
    stop("'tau' must be a function when boot_rate = \"custom\".")
  }
  out <- tau(k)
  if (!is.numeric(out) || length(out) != 1L || !is.finite(out) || out <= 0) {
    stop("'tau' must return a single positive finite numeric value for ", label, ".")
  }
  as.numeric(out)
}

.lpmec_validate_root_calibration <- function(boot_ci_type,
                                             bootstrap_method,
                                             n_boot,
                                             boot_calibration_n,
                                             boot_calibration_inner_n_boot,
                                             boot_calibration_cut_grid,
                                             boot_calibration_tail,
                                             boot_calibration_seed,
                                             alpha) {
  boot_calibration_tail <- match.arg(boot_calibration_tail, c("separate", "equal_tail"))
  if (boot_ci_type != "root_calibrated") {
    return(list(
      n = as.integer(boot_calibration_n),
      inner_n_boot = boot_calibration_inner_n_boot,
      cut_grid = sort(unique(as.numeric(boot_calibration_cut_grid))),
      tail = boot_calibration_tail,
      seed = boot_calibration_seed
    ))
  }

  if (!bootstrap_method %in% c("m_out_of_n", "subsampling")) {
    stop("boot_ci_type = \"root_calibrated\" requires bootstrap_method = \"subsampling\" or \"m_out_of_n\".")
  }
  if (!is.numeric(n_boot) ||
      length(n_boot) != 1L ||
      !is.finite(n_boot) ||
      n_boot != floor(n_boot) ||
      n_boot < 2L) {
    stop("boot_ci_type = \"root_calibrated\" requires n_boot >= 2.")
  }
  if (!is.numeric(boot_calibration_n) ||
      length(boot_calibration_n) != 1L ||
      !is.finite(boot_calibration_n) ||
      boot_calibration_n != floor(boot_calibration_n) ||
      boot_calibration_n < 1L) {
    stop("'boot_calibration_n' must be a single positive integer.")
  }
  if (is.null(boot_calibration_inner_n_boot)) {
    boot_calibration_inner_n_boot <- n_boot
  }
  if (!is.numeric(boot_calibration_inner_n_boot) ||
      length(boot_calibration_inner_n_boot) != 1L ||
      !is.finite(boot_calibration_inner_n_boot) ||
      boot_calibration_inner_n_boot != floor(boot_calibration_inner_n_boot) ||
      boot_calibration_inner_n_boot < 2L) {
    stop("'boot_calibration_inner_n_boot' must be NULL or a single integer greater than or equal to 2.")
  }
  if (!is.numeric(boot_calibration_cut_grid) ||
      length(boot_calibration_cut_grid) < 1L ||
      any(!is.finite(boot_calibration_cut_grid)) ||
      any(boot_calibration_cut_grid <= 0) ||
      any(boot_calibration_cut_grid > alpha)) {
    stop("'boot_calibration_cut_grid' must contain finite values greater than 0 and less than or equal to boot_alpha.")
  }
  if (!is.null(boot_calibration_seed) &&
      (!is.numeric(boot_calibration_seed) ||
       length(boot_calibration_seed) != 1L ||
       !is.finite(boot_calibration_seed))) {
    stop("'boot_calibration_seed' must be NULL or a single finite numeric value.")
  }

  list(
    n = as.integer(boot_calibration_n),
    inner_n_boot = as.integer(boot_calibration_inner_n_boot),
    cut_grid = sort(unique(as.numeric(boot_calibration_cut_grid))),
    tail = boot_calibration_tail,
    seed = boot_calibration_seed
  )
}

.lpmec_select_root_calibration_cutoffs <- function(calibration_coverage,
                                                   alpha = 0.05,
                                                   tail = c("separate", "equal_tail")) {
  tail <- match.arg(tail)
  required <- c("field", "cut", "lower_coverage", "upper_coverage", "twosided_coverage")
  if (!all(required %in% names(calibration_coverage))) {
    stop("'calibration_coverage' must contain columns: ", paste(required, collapse = ", "), ".")
  }
  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop("'alpha' must be a single value in (0, 1).")
  }

  fields <- unique(as.character(calibration_coverage$field))
  out <- lapply(fields, function(field_name) {
    x <- calibration_coverage[as.character(calibration_coverage$field) == field_name, , drop = FALSE]
    x <- x[order(x$cut), , drop = FALSE]
    valid_cuts <- x$cut[is.finite(x$cut)]
    if (length(valid_cuts) < 1L) {
      return(data.frame(
        field = field_name,
        root_left_tail = NA_real_,
        root_right_tail = NA_real_,
        selected_lower_coverage = NA_real_,
        selected_upper_coverage = NA_real_,
        selected_twosided_coverage = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    choose_largest <- function(coverage, target) {
      ok <- which(is.finite(coverage) & coverage >= target)
      if (length(ok) < 1L) {
        return(min(valid_cuts))
      }
      max(x$cut[ok], na.rm = TRUE)
    }

    if (tail == "equal_tail") {
      chosen <- choose_largest(x$twosided_coverage, 1 - alpha)
      left_tail <- chosen
      right_tail <- chosen
    } else {
      left_tail <- choose_largest(x$upper_coverage, 1 - alpha / 2)
      right_tail <- choose_largest(x$lower_coverage, 1 - alpha / 2)
    }

    selected_row <- function(cut) {
      which.min(abs(x$cut - cut))
    }
    left_row <- selected_row(left_tail)
    right_row <- selected_row(right_tail)
    two_row <- selected_row(if (tail == "equal_tail") left_tail else min(left_tail, right_tail))
    data.frame(
      field = field_name,
      root_left_tail = as.numeric(left_tail),
      root_right_tail = as.numeric(right_tail),
      selected_lower_coverage = as.numeric(x$lower_coverage[right_row]),
      selected_upper_coverage = as.numeric(x$upper_coverage[left_row]),
      selected_twosided_coverage = as.numeric(x$twosided_coverage[two_row]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

.lpmec_apply_root_tail_cutoffs <- function(summary,
                                           n,
                                           rate = "sqrt_n",
                                           tau = NULL,
                                           root_left_tail,
                                           root_right_tail) {
  if (is.null(summary$root_draws) || length(summary$root_draws) < 2L ||
      !is.finite(summary$estimate) ||
      !is.finite(root_left_tail) ||
      !is.finite(root_right_tail)) {
    summary$root_left_tail <- root_left_tail
    summary$root_right_tail <- root_right_tail
    return(summary)
  }
  if (root_left_tail <= 0 || root_left_tail >= 1 ||
      root_right_tail <= 0 || root_right_tail >= 1) {
    stop("'root_left_tail' and 'root_right_tail' must be in (0, 1).")
  }
  rate_n <- .lpmec_rate_value(n, rate = rate, tau = tau, label = "n")
  root_draws <- as.numeric(summary$root_draws)
  summary$lower <- as.numeric(
    summary$estimate -
      stats::quantile(root_draws, 1 - root_right_tail, na.rm = TRUE, names = FALSE) / rate_n
  )
  summary$upper <- as.numeric(
    summary$estimate -
      stats::quantile(root_draws, root_left_tail, na.rm = TRUE, names = FALSE) / rate_n
  )
  summary$root_left_tail <- as.numeric(root_left_tail)
  summary$root_right_tail <- as.numeric(root_right_tail)
  summary
}

.lpmec_summarize_resampling <- function(theta0,
                                        theta_boot,
                                        n,
                                        m,
                                        bootstrap_method,
                                        boot_ci_type,
                                        alpha = 0.05,
                                        rate = "sqrt_n",
                                        tau = NULL,
                                        root_left_tail = NULL,
                                        root_right_tail = NULL) {
  bootstrap_method <- match.arg(bootstrap_method, c("n_out_of_n", "m_out_of_n", "subsampling"))
  boot_ci_type <- match.arg(boot_ci_type, c("root", "root_calibrated", "percentile"))
  rate <- match.arg(rate, c("sqrt_n", "custom"))
  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop("'alpha' must be a single value in (0, 1).")
  }
  if (is.null(root_left_tail)) {
    root_left_tail <- alpha / 2
  }
  if (is.null(root_right_tail)) {
    root_right_tail <- alpha / 2
  }
  if (!is.numeric(root_left_tail) ||
      length(root_left_tail) != 1L ||
      !is.finite(root_left_tail) ||
      root_left_tail <= 0 ||
      root_left_tail >= 1) {
    stop("'root_left_tail' must be NULL or a single value in (0, 1).")
  }
  if (!is.numeric(root_right_tail) ||
      length(root_right_tail) != 1L ||
      !is.finite(root_right_tail) ||
      root_right_tail <= 0 ||
      root_right_tail >= 1) {
    stop("'root_right_tail' must be NULL or a single value in (0, 1).")
  }

  theta0 <- as.numeric(theta0)[1L]
  theta_boot <- as.numeric(theta_boot)
  finite <- is.finite(theta_boot)
  n_failed <- sum(!finite)
  if (n_failed > 0L && any(finite)) {
    warning("Dropping ", n_failed, " non-finite resampling draw(s).", call. = FALSE)
  }
  theta_boot <- theta_boot[finite]
  n_draws <- length(theta_boot)

  empty <- list(
    estimate = theta0,
    se = NA_real_,
    lower = NA_real_,
    upper = NA_real_,
    root_draws = numeric(0L),
    n_draws = n_draws,
    n_failed = n_failed,
    success_rate = if ((n_draws + n_failed) > 0L) n_draws / (n_draws + n_failed) else NA_real_,
    root_left_tail = if (boot_ci_type == "percentile") NA_real_ else as.numeric(root_left_tail),
    root_right_tail = if (boot_ci_type == "percentile") NA_real_ else as.numeric(root_right_tail)
  )
  if (!is.finite(theta0) || n_draws < 2L) {
    return(empty)
  }

  rate_m <- .lpmec_rate_value(m, rate = rate, tau = tau, label = "m")
  rate_n <- .lpmec_rate_value(n, rate = rate, tau = tau, label = "n")

  if (boot_ci_type == "percentile") {
    se <- stats::sd(theta_boot)
    lower <- stats::quantile(theta_boot, alpha / 2, na.rm = TRUE, names = FALSE)
    upper <- stats::quantile(theta_boot, 1 - alpha / 2, na.rm = TRUE, names = FALSE)
    root_draws <- rate_m * (theta_boot - theta0)
  } else {
    root_draws <- rate_m * (theta_boot - theta0)
    se <- stats::sd(root_draws, na.rm = TRUE) / rate_n
    lower <- theta0 - stats::quantile(root_draws, 1 - root_right_tail, na.rm = TRUE, names = FALSE) / rate_n
    upper <- theta0 - stats::quantile(root_draws, root_left_tail, na.rm = TRUE, names = FALSE) / rate_n
  }

  list(
    estimate = theta0,
    se = as.numeric(se),
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    root_draws = as.numeric(root_draws),
    n_draws = n_draws,
    n_failed = n_failed,
    success_rate = n_draws / (n_draws + n_failed),
    root_left_tail = if (boot_ci_type == "percentile") NA_real_ else as.numeric(root_left_tail),
    root_right_tail = if (boot_ci_type == "percentile") NA_real_ else as.numeric(root_right_tail)
  )
}

.lpmec_aggregate_numeric_by_boot <- function(values,
                                             boot_index,
                                             aggregation_fn,
                                             valid = NULL) {
  values <- as.numeric(values)
  boot_index <- as.integer(boot_index)
  if (length(values) != length(boot_index)) {
    stop("'values' and 'boot_index' must have the same length.")
  }
  if (is.null(valid)) {
    valid <- rep(TRUE, length(values))
  }
  valid <- as.logical(valid)
  if (length(valid) != length(values)) {
    stop("'valid' must have the same length as 'values'.")
  }
  values[!valid] <- NA_real_
  boots <- sort(unique(boot_index))
  out <- vapply(boots, function(boot) {
    x <- values[boot_index == boot]
    x <- x[is.finite(x)]
    if (length(x) < 1L) {
      return(NA_real_)
    }
    as.numeric(aggregation_fn(x))
  }, numeric(1L))
  names(out) <- as.character(boots)
  out
}

.lpmec_cbind_intermediary <- function(existing, new) {
  existing_mat <- if (is.null(dim(existing))) {
    matrix(existing, ncol = 1L)
  } else {
    existing
  }
  new_mat <- if (is.null(dim(new))) {
    matrix(new, ncol = 1L)
  } else {
    new
  }
  target_nrow <- max(nrow(existing_mat), nrow(new_mat))

  pad_to <- function(x, n) {
    if (nrow(x) == n) {
      return(x)
    }
    pad_n <- n - nrow(x)
    pad_value <- if (is.character(x)) {
      NA_character_
    } else if (is.logical(x)) {
      NA
    } else {
      NA_real_
    }
    pad <- matrix(pad_value, nrow = pad_n, ncol = ncol(x))
    colnames(pad) <- colnames(x)
    rbind(x, pad)
  }

  cbind(pad_to(existing_mat, target_nrow), pad_to(new_mat, target_nrow))
}
