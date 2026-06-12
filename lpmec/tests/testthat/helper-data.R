make_lpmec_test_data <- function(n = 80L, p = 6L, seed = 123L) {
  set.seed(seed)
  list(
    Y = rnorm(n),
    obs = as.data.frame(matrix(sample(c(0, 1), n * p, replace = TRUE), ncol = p))
  )
}
