expect_exported_boundary_error <- function(expr, message) {
  err <- tryCatch(expr, error = identity)
  expect_s3_class(err, "error")
  expect_identical(conditionMessage(err), message)
  expect_null(conditionCall(err))
}

test_that("vuongtest validates option arguments at the exported boundary", {
  nested_message <- "Argument 'nested' must be a single logical value (TRUE/FALSE)."
  for (value in list(logical(), NA, c(TRUE, FALSE), 1, "yes", NULL)) {
    expect_exported_boundary_error(
      vuongtest(NULL, NULL, nested = value),
      nested_message
    )
  }

  adj_message <- "Argument 'adj' must be a single character string ('none', 'aic', or 'bic')."
  for (value in list(character(), NA_character_, c("none", "aic"), "AIC", "", "aic ", "other", 1, NULL)) {
    expect_exported_boundary_error(
      vuongtest(NULL, NULL, adj = value),
      adj_message
    )
  }
})

test_that("icci validates confidence levels at the exported boundary", {
  conf_message <- "Argument 'conf.level' must be a single numeric value between 0 and 1."
  for (value in list(numeric(), NA_real_, NaN, Inf, -Inf, c(0.9, 0.95), 0, 1, -0.1, 1.1, "0.95", NULL)) {
    expect_exported_boundary_error(
      icci(NULL, NULL, conf.level = value),
      conf_message
    )
  }
})

test_that("exported likelihood callbacks must be functions", {
  callback_message <- "Arguments 'll1' and 'll2' must be functions."
  for (value in list(NULL, "llcont", 1, TRUE, list())) {
    expect_exported_boundary_error(
      vuongtest(NULL, NULL, ll1 = value),
      callback_message
    )
    expect_exported_boundary_error(
      vuongtest(NULL, NULL, ll2 = value),
      callback_message
    )
    expect_exported_boundary_error(
      icci(NULL, NULL, ll1 = value),
      callback_message
    )
    expect_exported_boundary_error(
      icci(NULL, NULL, ll2 = value),
      callback_message
    )
  }
})

test_that("vuongtest validates optional score and covariance callbacks", {
  score_message <- "Arguments 'score1' and 'score2' must be functions or NULL."
  for (value in list("score", 1, TRUE, list())) {
    expect_exported_boundary_error(
      vuongtest(NULL, NULL, score1 = value),
      score_message
    )
    expect_exported_boundary_error(
      vuongtest(NULL, NULL, score2 = value),
      score_message
    )
  }

  vc_message <- "Arguments 'vc1' and 'vc2' must be functions."
  for (value in list(NULL, "vcov", 1, TRUE, list())) {
    expect_exported_boundary_error(
      vuongtest(NULL, NULL, vc1 = value),
      vc_message
    )
    expect_exported_boundary_error(
      vuongtest(NULL, NULL, vc2 = value),
      vc_message
    )
  }
})
