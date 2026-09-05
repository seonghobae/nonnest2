context("llcont.polr indexing")


test_that("unweighted polr contributions retain one value per fitted row", {
  with_test_packages("MASS", {
    fit <- polr(Sat ~ Infl + Type + Cont, data = housing, Hess = TRUE)
    contributions <- llcont(fit)

    expect_length(contributions, nrow(fit$fitted.values))
    expect_equal(sum(contributions), as.numeric(logLik(fit)))
  })
})


test_that("polr direct indexing uses fitted-row position after subsetting", {
  with_test_packages("MASS", {
    retained <- setdiff(seq_len(nrow(housing)), c(2L, 5L, 8L))
    fit <- polr(
      Sat ~ Infl + Type + Cont,
      data = housing[retained, , drop = FALSE],
      Hess = TRUE
    )
    response_codes <- as.numeric(unclass(model.response(fit$model)))
    expected <- log(
      fit$fitted.values[cbind(seq_along(response_codes), response_codes)]
    )

    expect_equal(unname(llcont(fit)), unname(expected))
    expect_equal(sum(llcont(fit)), as.numeric(logLik(fit)))
  })
})
