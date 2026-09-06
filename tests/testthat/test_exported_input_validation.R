test_that("vuongtest validates option arguments at the exported boundary", {
  nested_message <- "Argument 'nested' must be a single logical value (TRUE/FALSE)."
  for (value in list(NA, c(TRUE, FALSE), 1, NULL)) {
    expect_error(vuongtest(NULL, NULL, nested = value), nested_message, fixed = TRUE)
  }

  adj_message <- "Argument 'adj' must be a single character string ('none', 'aic', or 'bic')."
  for (value in list(NA_character_, c("none", "aic"), "AIC", 1, NULL)) {
    expect_error(vuongtest(NULL, NULL, adj = value), adj_message, fixed = TRUE)
  }
})

test_that("icci validates confidence levels at the exported boundary", {
  conf_message <- "Argument 'conf.level' must be a single numeric value between 0 and 1."
  for (value in list(NA_real_, c(0.9, 0.95), 0, 1, -0.1, 1.1, "0.95", NULL)) {
    expect_error(icci(NULL, NULL, conf.level = value), conf_message, fixed = TRUE)
  }
})
