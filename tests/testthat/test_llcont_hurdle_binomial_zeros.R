context("llcont hurdle and binomial zeros")

test_that("llcont hurdle negative binomial works", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("MASS")
  data(bioChemists, package = "pscl")

  # Ensure hurdle runs are working and identical to logLik
  bio_hurdle_nb <- suppressWarnings(
    pscl::hurdle(art ~ fem + mar + phd + ment, data=bioChemists, dist="negbin", zero.dist="negbin")
  )
  expect_equal(sum(llcont(bio_hurdle_nb)), as.numeric(logLik(bio_hurdle_nb)))

  bio_hurdle_pois <- suppressWarnings(
    pscl::hurdle(art ~ fem + mar + phd + ment, data=bioChemists, dist="poisson", zero.dist="poisson")
  )
  expect_equal(sum(llcont(bio_hurdle_pois)), as.numeric(logLik(bio_hurdle_pois)))
})
