context("icci_conf_level")

test_that("icci throws error for invalid conf.level", {
  library(MASS)
  house1 <- glm(Freq ~ Infl + Type + Cont, family=poisson, data=housing)
  house2 <- glm(Freq ~ Infl + Sat, family=poisson, data=housing)

  expect_error(icci(house2, house1, conf.level = "invalid"), "conf.level must be a single numeric value between 0 and 1.")
  expect_error(icci(house2, house1, conf.level = -0.5), "conf.level must be a single numeric value between 0 and 1.")
  expect_error(icci(house2, house1, conf.level = 1.5), "conf.level must be a single numeric value between 0 and 1.")
  expect_error(icci(house2, house1, conf.level = c(0.95, 0.99)), "conf.level must be a single numeric value between 0 and 1.")
  expect_error(icci(house2, house1, conf.level = NA), "conf.level must be a single numeric value between 0 and 1.")
})
