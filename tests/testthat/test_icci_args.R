test_that("icci sanitizes unvalidated conf.level errors", {
  dat <- data.frame(y = c(1, 2, 3, 4), x = c(1, 2, 3, 4))
  model_a <- lm(y ~ x, data = dat)
  model_b <- lm(y ~ 1, data = dat)

  err1 <- tryCatch(icci(model_a, model_b, conf.level = "a"), error = identity)
  expect_s3_class(err1, "error")
  expect_identical(conditionMessage(err1), "conf.level must be a numeric scalar between 0 and 1")
  expect_null(conditionCall(err1))

  err2 <- tryCatch(icci(model_a, model_b, conf.level = c(0.95, 0.99)), error = identity)
  expect_s3_class(err2, "error")
  expect_identical(conditionMessage(err2), "conf.level must be a numeric scalar between 0 and 1")

  err3 <- tryCatch(icci(model_a, model_b, conf.level = NA_real_), error = identity)
  expect_s3_class(err3, "error")
  expect_identical(conditionMessage(err3), "conf.level must be a numeric scalar between 0 and 1")

  err4 <- tryCatch(icci(model_a, model_b, conf.level = Inf), error = identity)
  expect_s3_class(err4, "error")
  expect_identical(conditionMessage(err4), "conf.level must be a numeric scalar between 0 and 1")
})
