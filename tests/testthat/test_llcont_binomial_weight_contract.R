context("binomial llcont weight contract")

test_that("zero prior weights remain zero-contribution observations", {
  data <- data.frame(
    outcome = c(0, 1, 0, 1, 1),
    predictor = c(-2, -1, 0, 1, 2),
    prior_weight = c(1, 2, 0, 3, 1)
  )

  fit <- glm(
    outcome ~ predictor,
    data = data,
    family = binomial(),
    weights = prior_weight
  )

  contributions <- llcont(fit)

  expect_length(contributions, nrow(data))
  expect_equal(contributions[3], 0)
  expect_equal(sum(contributions), as.numeric(logLik(fit)), tolerance = 1e-10)
})
