context("weighted nls llcont contract")

test_that("weighted nls contributions sum to stats logLik", {
  DNase1 <- subset(DNase, Run == 1)
  case_weights <- rep(c(0.5, 1, 2, 4), length.out = nrow(DNase1))
  fit <- nls(
    density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
    data = DNase1,
    weights = case_weights,
    start = list(Asym = 3, xmid = 0, scal = 1)
  )

  expect_equal(sum(llcont(fit)), as.numeric(logLik(fit)), tolerance = 1e-8)
})

test_that("zero-weight nls observations make no likelihood contribution", {
  DNase1 <- subset(DNase, Run == 1)
  case_weights <- rep(c(0, 1, 2, 4), length.out = nrow(DNase1))
  fit <- nls(
    density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
    data = DNase1,
    weights = case_weights,
    start = list(Asym = 3, xmid = 0, scal = 1)
  )

  contributions <- llcont(fit)
  expect_equal(contributions[case_weights == 0], rep(0, sum(case_weights == 0)))
  expect_equal(sum(contributions), as.numeric(logLik(fit)), tolerance = 1e-8)
})
