reference_hurdle_llcont <- function(x) {
  X <- model.matrix(x, model = "count")
  Z <- model.matrix(x, model = "zero")
  Y <- x$y
  n <- length(Y)
  kx <- NCOL(X)
  kz <- NCOL(Z)
  Y0 <- Y <= 0
  Y1 <- Y > 0
  weights <- if (is.null(w <- weights(x))) rep.int(1L, n) else w
  offsetx <- if (is.null(x$offset$count)) rep.int(0, n) else x$offset$count
  offsetz <- if (is.null(x$offset$zero)) rep.int(0, n) else x$offset$zero

  zero_poisson <- function(parms) {
    mu <- as.vector(exp(Z %*% parms + offsetz))
    loglik0 <- -mu
    Y0 * weights * loglik0 +
      ifelse(Y1, weights * log(1 - exp(loglik0)), 0)
  }
  count_poisson <- function(parms) {
    mu <- Y1 * as.vector(exp(X %*% parms + offsetx))
    loglik0 <- -mu
    loglik1 <- Y1 * dpois(Y, lambda = mu, log = TRUE)
    Y1 * weights * loglik1 -
      ifelse(Y1, weights * log(1 - exp(loglik0)), 0)
  }
  zero_negbin <- function(parms) {
    mu <- as.vector(exp(Z %*% parms[seq_len(kz)] + offsetz))
    theta <- exp(parms[kz + 1L])
    loglik0 <- suppressWarnings(
      dnbinom(0, size = theta, mu = mu, log = TRUE)
    )
    Y0 * weights * loglik0 +
      ifelse(Y1, weights * log(1 - exp(loglik0)), 0)
  }
  count_negbin <- function(parms) {
    mu <- as.vector(exp(X %*% parms[seq_len(kx)] + offsetx))
    theta <- exp(parms[kx + 1L])
    loglik0 <- suppressWarnings(
      dnbinom(0, size = theta, mu = mu, log = TRUE)
    )
    loglik1 <- suppressWarnings(
      dnbinom(Y, size = theta, mu = mu, log = TRUE)
    )
    ifelse(
      Y1,
      weights * loglik1 - weights * log(1 - exp(loglik0)),
      0
    )
  }
  zero_binomial <- function(parms) {
    mu <- as.vector(x$linkinv(Z %*% parms + offsetz))
    Y0 * weights * log(1 - mu) + Y1 * weights * log(mu)
  }

  zero_geometric <- function(parms) zero_negbin(c(parms, 0))
  count_geometric <- function(parms) count_negbin(c(parms, 0))
  count_dist <- switch(
    x$dist$count,
    poisson = count_poisson,
    geometric = count_geometric,
    negbin = count_negbin
  )
  zero_dist <- switch(
    x$dist$zero,
    poisson = zero_poisson,
    geometric = zero_geometric,
    negbin = zero_negbin,
    binomial = zero_binomial
  )

  if (x$separate) {
    count_dist(c(
      x$coefficients$count,
      if (x$dist$count == "negbin") log(x$theta["count"]) else NULL
    )) + zero_dist(c(
      x$coefficients$zero,
      if (x$dist$zero == "negbin") log(x$theta["zero"]) else NULL
    ))
  } else {
    parms <- c(
      x$coefficients$count,
      if (x$dist$count == "negbin") log(x$theta["count"]) else NULL,
      x$coefficients$zero,
      if (x$dist$zero == "negbin") log(x$theta["zero"]) else NULL
    )
    count_end <- kx + (x$dist$count == "negbin")
    zero_end <- count_end + kz + (x$dist$zero == "negbin")
    count_dist(parms[seq_len(count_end)]) +
      zero_dist(parms[(count_end + 1L):zero_end])
  }
}

expect_hurdle_vector_matches_reference <- function(fit) {
  expected <- reference_hurdle_llcont(fit)
  actual <- llcont(fit)

  expect_identical(length(actual), length(expected))
  expect_identical(attributes(actual), attributes(expected))
  expect_equal(actual, expected, tolerance = 1e-12)
}

test_that("hurdle likelihood optimization preserves every observation", {
  with_test_packages("pscl", {
    poisson_fit <- hurdle(
      art ~ fem + ment | fem + ment,
      data = bioChemists,
      dist = "poisson",
      zero.dist = "poisson",
      model = TRUE
    )
    negbin_fit <- hurdle(
      art ~ fem + ment | fem + ment,
      data = bioChemists,
      dist = "negbin",
      zero.dist = "negbin",
      model = TRUE
    )

    scalar_weight_fit <- poisson_fit
    scalar_weight_fit$weights <- 2
    expect_hurdle_vector_matches_reference(scalar_weight_fit)

    observation_weight_fit <- negbin_fit
    observation_weight_fit$weights <- seq_len(length(negbin_fit$y)) /
      length(negbin_fit$y)
    expect_hurdle_vector_matches_reference(observation_weight_fit)

    missing_outcome_fit <- poisson_fit
    missing_outcome_fit$y[c(2, 5)] <- NA_real_
    expect_hurdle_vector_matches_reference(missing_outcome_fit)
  })
})
