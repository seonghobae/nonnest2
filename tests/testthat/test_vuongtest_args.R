test_that("vuongtest sanitizes unvalidated arguments errors", {
  dat <- data.frame(y = c(1, 2, 3, 4), x = c(1, 2, 3, 4))
  model_a <- lm(y ~ x, data = dat)
  model_b <- lm(y ~ 1, data = dat)

  err1 <- tryCatch(vuongtest(model_a, model_b, nested = "yes"), error = identity)
  expect_s3_class(err1, "error")
  expect_identical(conditionMessage(err1), "nested must be a logical scalar (TRUE or FALSE)")
  expect_null(conditionCall(err1))

  err2 <- tryCatch(vuongtest(model_a, model_b, nested = c(TRUE, FALSE)), error = identity)
  expect_s3_class(err2, "error")
  expect_identical(conditionMessage(err2), "nested must be a logical scalar (TRUE or FALSE)")

  err3 <- tryCatch(vuongtest(model_a, model_b, adj = c("none", "aic")), error = identity)
  expect_s3_class(err3, "error")
  expect_identical(conditionMessage(err3), "adj must be one of 'none', 'aic', or 'bic'")
  expect_null(conditionCall(err3))

  err4 <- tryCatch(vuongtest(model_a, model_b, adj = "what"), error = identity)
  expect_s3_class(err4, "error")
  expect_identical(conditionMessage(err4), "adj must be one of 'none', 'aic', or 'bic'")

  err5 <- tryCatch(vuongtest(model_a, model_b, adj = factor("none")), error = identity)
  expect_s3_class(err5, "error")
  expect_identical(conditionMessage(err5), "adj must be one of 'none', 'aic', or 'bic'")
})
