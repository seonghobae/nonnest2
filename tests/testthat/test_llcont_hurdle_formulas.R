test_that("hurdle ifelse optimization correctness", {
  # Isolating the logic to compare unoptimized (ifelse) vs optimized
  # zeroPoisson
  run_zeroPoisson <- function(Z, parms, offsetz, weights, Y0, Y1) {
    mu <- as.vector(exp(Z %*% parms + offsetz))
    loglik0 <- -mu
    orig <- Y0 * weights * loglik0 + ifelse(Y1, weights * log(1 - exp(loglik0)), 0)

    res_Y1 <- Y1 * 0
    cond <- Y1; cond[is.na(cond)] <- FALSE
    if (any(cond)) {
      w_c <- if (length(weights) == 1) rep_len(weights, length(cond))[cond] else weights[cond]
      res_Y1[cond] <- w_c * log(1 - exp(loglik0[cond]))
    }
    opt <- Y0 * weights * loglik0 + res_Y1

    expect_equal(orig, opt)
  }

  # countPoisson
  run_countPoisson <- function(X, parms, offsetx, weights, Y1, Y) {
    mu <- Y1 * as.vector(exp(X %*% parms + offsetx))
    loglik0 <- -mu
    loglik1 <- Y1 * dpois(Y, lambda = mu, log = TRUE)
    orig <- Y1 * weights * loglik1 - ifelse(Y1, weights * log(1 - exp(loglik0)), 0)

    res_Y1 <- Y1 * 0
    cond <- Y1; cond[is.na(cond)] <- FALSE
    if (any(cond)) {
      w_c <- if (length(weights) == 1) rep_len(weights, length(cond))[cond] else weights[cond]
      res_Y1[cond] <- w_c * log(1 - exp(loglik0[cond]))
    }
    opt <- Y1 * weights * loglik1 - res_Y1

    expect_equal(orig, opt)
  }

  # test cases setup
  n <- 100
  Z <- matrix(rnorm(n*2), n, 2)
  X <- matrix(rnorm(n*2), n, 2)
  parms <- c(0.5, -0.5)
  offsetx <- rep(0, n)
  offsetz <- rep(0, n)

  # Case 1: mixed
  Y <- rpois(n, 1.5)
  Y0 <- Y <= 0
  Y1 <- Y > 0
  weights <- 1
  run_zeroPoisson(Z, parms, offsetz, weights, Y0, Y1)
  run_countPoisson(X, parms, offsetx, weights, Y1, Y)

  # Case 2: all zeros
  Y <- rep(0, n)
  Y0 <- Y <= 0
  Y1 <- Y > 0
  run_zeroPoisson(Z, parms, offsetz, weights, Y0, Y1)
  run_countPoisson(X, parms, offsetx, weights, Y1, Y)

  # Case 3: all pos
  Y <- rep(2, n)
  Y0 <- Y <= 0
  Y1 <- Y > 0
  run_zeroPoisson(Z, parms, offsetz, weights, Y0, Y1)
  run_countPoisson(X, parms, offsetx, weights, Y1, Y)

  # Case 4: NAs
  Y <- c(rpois(n-10, 1.5), rep(NA, 10))
  Y0 <- Y <= 0
  Y1 <- Y > 0
  run_zeroPoisson(Z, parms, offsetz, weights, Y0, Y1)
  run_countPoisson(X, parms, offsetx, weights, Y1, Y)

  # Case 5: per obs weights
  weights <- runif(n)
  run_zeroPoisson(Z, parms, offsetz, weights, Y0, Y1)
  run_countPoisson(X, parms, offsetx, weights, Y1, Y)
})
