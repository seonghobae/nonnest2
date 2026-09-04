test_that("vuongtest rejects malformed public arguments before object inspection", {
  invalid_cases <- list(
    list(args = list(nested = NA), message = "nested must be a single logical value"),
    list(args = list(nested = c(TRUE, FALSE)), message = "nested must be a single logical value"),
    list(args = list(adj = NA_character_), message = 'adj must be one of "none", "aic", or "bic"'),
    list(args = list(adj = "unknown"), message = 'adj must be one of "none", "aic", or "bic"'),
    list(args = list(ll1 = "llcont"), message = "ll1 and ll2 must be functions"),
    list(args = list(score1 = "score"), message = "score1 and score2 must be functions or NULL"),
    list(args = list(vc1 = "vcov"), message = "vc1 and vc2 must be functions")
  )

  for (case in invalid_cases) {
    err <- tryCatch(
      do.call(vuongtest, c(list(object1 = NULL, object2 = NULL), case$args)),
      error = identity
    )

    expect_s3_class(err, "error")
    expect_identical(conditionMessage(err), case$message)
    expect_null(conditionCall(err))
  }
})


test_that("icci rejects malformed public arguments before object inspection", {
  invalid_cases <- list(
    list(args = list(conf.level = NA_real_), message = "conf.level must be a single numeric value strictly between 0 and 1"),
    list(args = list(conf.level = c(0.9, 0.95)), message = "conf.level must be a single numeric value strictly between 0 and 1"),
    list(args = list(conf.level = 0), message = "conf.level must be a single numeric value strictly between 0 and 1"),
    list(args = list(conf.level = 1), message = "conf.level must be a single numeric value strictly between 0 and 1"),
    list(args = list(conf.level = "0.95"), message = "conf.level must be a single numeric value strictly between 0 and 1"),
    list(args = list(ll2 = "llcont"), message = "ll1 and ll2 must be functions")
  )

  for (case in invalid_cases) {
    err <- tryCatch(
      do.call(icci, c(list(object1 = NULL, object2 = NULL), case$args)),
      error = identity
    )

    expect_s3_class(err, "error")
    expect_identical(conditionMessage(err), case$message)
    expect_null(conditionCall(err))
  }
})
