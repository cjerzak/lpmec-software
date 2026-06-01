# Regression tests for edge cases found during bug analysis

skip_on_cran()

make_regression_data <- function(n = 80L) {
  i <- seq_len(n)
  obs <- data.frame(
    V1 = i %% 2L,
    V2 = (i + 1L) %% 2L,
    V3 = as.integer(i %% 3L == 0L),
    V4 = as.integer(i %% 4L <= 1L),
    V5 = as.integer(i %% 5L <= 2L),
    V6 = as.integer(i %% 7L <= 3L)
  )
  Y <- as.numeric(scale(rowMeans(obs))) + stats::rnorm(n, sd = 0.1)
  list(Y = Y, obs = obs)
}

test_that("PCA estimation handles partial missingness and constant columns", {
  set.seed(101)
  dat <- make_regression_data()
  dat$obs$V1 <- 1L
  dat$obs[1:5, "V2"] <- NA

  expect_warning(
    lpmec:::.lpmec_prepare_pca_observables(dat$obs),
    "Dropping"
  )
  suppressWarnings(
    res_one <- lpmec_onerun(dat$Y, dat$obs, estimation_method = "pca")
  )
  expect_s3_class(res_one, "lpmec_onerun")
  expect_true(all(is.finite(res_one$x_est1)))
  expect_true(all(is.finite(res_one$x_est2)))

  suppressWarnings(
    res_boot <- lpmec(dat$Y, dat$obs, n_boot = 0, n_partition = 1,
                      estimation_method = "pca")
  )
  expect_s3_class(res_boot, "lpmec")
  expect_true(is.finite(res_boot$ols_coef))
})

test_that("PCA estimation fails clearly when inputs cannot support PCA", {
  set.seed(102)
  dat <- make_regression_data()
  constant_obs <- dat$obs
  constant_obs[] <- 1L

  expect_warning(
    expect_error(
      lpmec_onerun(dat$Y, constant_obs, estimation_method = "pca"),
      "no informative columns"
    ),
    "Dropping"
  )

  missing_row_obs <- dat$obs
  missing_row_obs[1, ] <- NA
  expect_error(
    lpmec_onerun(dat$Y, missing_row_obs, estimation_method = "pca"),
    "rows with no observed values"
  )
})

test_that("PC1 orientation handles constant columns and errors on all-noninformative inputs", {
  set.seed(103)
  dat <- make_regression_data()
  dat$obs$V1 <- 1L

  expect_warning(
    signs <- infer_orientation_signs(observables = dat$obs, method = "PC1"),
    "Dropping"
  )
  expect_length(signs, ncol(dat$obs))
  expect_true(all(signs %in% c(1, -1)))
  expect_equal(signs[1], 1)

  constant_obs <- dat$obs
  constant_obs[] <- 1L
  expect_warning(
    expect_error(
      infer_orientation_signs(observables = constant_obs, method = "PC1"),
      "no informative columns"
    ),
    "Dropping"
  )
})

test_that("observable grouping validation catches impossible split-half inputs", {
  set.seed(104)
  dat <- make_regression_data()

  expect_error(
    lpmec_onerun(dat$Y, dat$obs, observables_groupings = rep("one", 6),
                 estimation_method = "averaging"),
    "At least 2 unique observable groupings"
  )
  expect_error(
    lpmec(dat$Y, dat$obs, observables_groupings = rep("one", 6),
          n_boot = 0, n_partition = 1, estimation_method = "averaging"),
    "At least 2 unique observable groupings"
  )

  expect_warning(
    res <- lpmec_onerun(dat$Y, dat$obs,
                        observables_groupings = rep(c("a", "b"), each = 3),
                        estimation_method = "averaging"),
    "fewer than 4 groupings"
  )
  expect_s3_class(res, "lpmec_onerun")
})

test_that("dummy expansion skips one-level columns and errors when none remain", {
  set.seed(105)
  dat <- make_regression_data()
  dat$obs$V1 <- 1L
  dat$obs[1, "V2"] <- NA

  expect_warning(
    expanded <- lpmec:::.lpmec_expand_observables_groupings(dat$obs),
    "Skipping"
  )
  expect_equal(nrow(expanded), nrow(dat$obs))
  suppressWarnings(
    res <- lpmec_onerun(dat$Y, dat$obs, estimation_method = "averaging",
                        make_observables_groupings = TRUE)
  )
  expect_s3_class(res, "lpmec_onerun")

  constant_obs <- dat$obs
  constant_obs[] <- 1L
  expect_warning(
    expect_error(
      lpmec_onerun(dat$Y, constant_obs, estimation_method = "averaging",
                   make_observables_groupings = TRUE),
      "no columns with at least two levels"
    ),
    "Skipping"
  )
})

test_that("lpmec retry exhaustion includes the last underlying error", {
  set.seed(106)
  dat <- make_regression_data()
  dat$obs[] <- 1L

  expect_error(
    suppressWarnings(suppressMessages(
      lpmec(dat$Y, dat$obs, n_boot = 0, n_partition = 1,
            estimation_method = "pca")
    )),
    "Last error: .*no informative columns"
  )
})

test_that("print methods return their input invisibly", {
  set.seed(107)
  dat <- make_regression_data()

  one_run <- lpmec_onerun(dat$Y, dat$obs, estimation_method = "averaging")
  one_visible <- withVisible(print(one_run))
  expect_false(one_visible$visible)
  expect_identical(one_visible$value, one_run)

  boot_run <- lpmec(dat$Y, dat$obs, n_boot = 0, n_partition = 1,
                    estimation_method = "averaging")
  boot_visible <- withVisible(print(boot_run))
  expect_false(boot_visible$visible)
  expect_identical(boot_visible$value, boot_run)
})

test_that("NumPyro MCMC code does not create diagnostic plots implicitly", {
  source_text <- paste(
    readLines(test_path("../../R/lpme_DoOneRun.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl(
    "plot(lpmec_env$np$array(PosteriorDraws$ability",
    source_text,
    fixed = TRUE
  ))
})
