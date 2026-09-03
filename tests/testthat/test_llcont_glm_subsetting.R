test_that("vectorized subsetting in llcont.glm matches ifelse exactly", {
  # Mock variables
  y_mat <- matrix(c(0, 1, 1, 0,
                    2, 0, 1, 1), ncol = 2)
  rownames(y_mat) <- c("r1", "r2", "r3", "r4")

  wt <- c(0.5, 1, 0.5, 2)
  mpreds <- c(0.2, 0.5, 0.8, 0.1)

  run_orig <- function(y, wt) {
    n <- rowSums(y)
    y_orig <- ifelse(n == 0, 0, y[, 1]/n)
    m <- if (any(n > 1)) n else wt
    wt_orig <- ifelse(m > 0, (wt/m), 0)
    list(y = y_orig, wt = wt_orig)
  }

  run_opt <- function(y, wt) {
    n <- rowSums(y)
    # y calculation
    y_opt <- y[, 1]/n
    y_opt[n == 0] <- 0
    # wt calculation
    m <- if (any(n > 1)) n else wt
    res_wt <- wt/m
    res_wt[m <= 0] <- 0
    wt_opt <- res_wt

    list(y = y_opt, wt = wt_opt)
  }

  # Standard test
  orig <- run_orig(y_mat, wt)
  opt <- run_opt(y_mat, wt)
  expect_identical(orig$y, opt$y)
  expect_identical(orig$wt, opt$wt)

  # NA test
  y_mat_na <- y_mat
  y_mat_na[1, 1] <- NA
  orig_na <- run_orig(y_mat_na, wt)
  opt_na <- run_opt(y_mat_na, wt)

  # Note: ifelse does not preserve names whereas vectorized division does preserve names
  # Therefore, using unname before comparing
  expect_identical(unname(orig_na$y), unname(opt_na$y))
  expect_identical(unname(orig_na$wt), unname(opt_na$wt))

  # m <= 0 test with scalar wt
  wt_scalar <- -1
  orig_m <- run_orig(y_mat, wt_scalar)
  opt_m <- run_opt(y_mat, wt_scalar)

  # Note: wt will recycle in ifelse in complex ways, while vector subsetting might not implicitly recycle as safely.
  # Let's compare equivalence in scalar cases
  expect_identical(unname(orig_m$y), unname(opt_m$y))
  expect_identical(unname(orig_m$wt), unname(opt_m$wt))

})
