test_that("icci rejects invalid confidence levels before model processing", {
  invalid_conf_levels <- list(
    numeric(0),
    c(0.9, 0.95),
    NA_real_,
    NaN,
    0,
    1,
    -0.1,
    1.1,
    -Inf,
    Inf,
    "0.95",
    TRUE
  )

  for (conf_level in invalid_conf_levels) {
    err <- tryCatch(
      icci(NULL, NULL, conf.level = conf_level),
      error = identity
    )

    expect_s3_class(err, "error")
    expect_identical(
      conditionMessage(err),
      "conf.level must be a single numeric value between 0 and 1."
    )
    expect_null(conditionCall(err))
  }
})
