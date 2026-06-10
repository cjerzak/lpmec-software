# Optional finite-partition median coverage check.
#
# This is not a CRAN test. It simulates P partition-specific asymptotically
# linear estimators with a common probability limit, then compares ordinary
# n-out-of-n percentile intervals with root-scaled subsampling intervals for
# the finite-partition median.

simulate_median_interval_once <- function(n,
                                          m,
                                          P = 9L,
                                          B = 399L,
                                          theta = 0,
                                          alpha = 0.05) {
  influence <- matrix(stats::rnorm(n * P), nrow = n, ncol = P)
  theta_parts <- theta + colMeans(influence) / sqrt(n)
  theta_hat <- stats::median(theta_parts)

  boot_n <- replicate(B, {
    idx <- sample.int(n, n, replace = TRUE)
    stats::median(theta + colMeans(influence[idx, , drop = FALSE]) / sqrt(n))
  })
  ci_n <- stats::quantile(boot_n, c(alpha / 2, 1 - alpha / 2), names = FALSE)

  root_m <- replicate(B, {
    idx <- sample.int(n, m, replace = FALSE)
    theta_m <- stats::median(theta + colMeans(influence[idx, , drop = FALSE]) / sqrt(m))
    sqrt(m) * (theta_m - theta_hat)
  })
  ci_root <- c(
    theta_hat - stats::quantile(root_m, 1 - alpha / 2, names = FALSE) / sqrt(n),
    theta_hat - stats::quantile(root_m, alpha / 2, names = FALSE) / sqrt(n)
  )

  c(
    n = n,
    m = m,
    P = P,
    percentile_cover = as.numeric(ci_n[1] <= theta && theta <= ci_n[2]),
    root_cover = as.numeric(ci_root[1] <= theta && theta <= ci_root[2]),
    percentile_width = diff(ci_n),
    root_width = diff(ci_root)
  )
}

run_median_m_out_of_n_experiment <- function(n_grid = c(200L, 500L),
                                             exponent_grid = c(0.60, 0.70, 0.80),
                                             P = 9L,
                                             R = 200L,
                                             B = 399L,
                                             seed = 123) {
  set.seed(seed)
  out <- list()
  k <- 0L
  for (n in n_grid) {
    for (exponent in exponent_grid) {
      m <- floor(n^exponent)
      m <- max(2L, min(m, n - 1L))
      draws <- replicate(
        R,
        simulate_median_interval_once(n = n, m = m, P = P, B = B),
        simplify = "matrix"
      )
      k <- k + 1L
      out[[k]] <- data.frame(
        n = n,
        m = m,
        m_ratio = m / n,
        exponent = exponent,
        P = P,
        percentile_coverage = mean(draws["percentile_cover", ]),
        root_coverage = mean(draws["root_cover", ]),
        percentile_width = mean(draws["percentile_width", ]),
        root_width = mean(draws["root_width", ])
      )
    }
  }
  do.call(rbind, out)
}

if (identical(environment(), globalenv())) {
  results <- run_median_m_out_of_n_experiment(R = 50L, B = 199L)
  print(results)
}
