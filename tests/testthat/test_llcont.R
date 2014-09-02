context("sum of llcont")


test_that("lavaan object", {
  if (isTRUE(require("lavaan"))) {
    HS.model <- 'visual  =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 speed   =~ x7 + x8 + x9 '
    fit1 <- cfa(HS.model, data=HolzingerSwineford1939)
    fit2 <- cfa(HS.model, data=HolzingerSwineford1939, group="school")

    expect_that(sum(llcont(fit1)), equals(as.numeric(logLik(fit1))))
    expect_that(sum(llcont(fit2)), equals(as.numeric(logLik(fit2))))
  }
})


test_that("glm object", {
  if (isTRUE(require("faraway")) && isTRUE(require("MASS"))) {
    ## binomial
    bin1 <- glm(formula=am ~ hp + wt, data=mtcars, family=binomial)
    bin2 <- glm(cbind(Menarche, Total-Menarche) ~ Age,
                family=binomial(logit), data=menarche)

    expect_that(sum(llcont(bin1)), equals(as.numeric(logLik(bin1))))
    expect_that(sum(llcont(bin2)), equals(as.numeric(logLik(bin2))))

    ## quasibinomial
    qbin1 <- glm(formula=am ~ hp + wt, data=mtcars, family=quasibinomial)
    qbin2 <- glm(cbind(Menarche, Total-Menarche) ~ Age,
                family=quasibinomial, data=menarche)

    expect_that(sum(llcont(qbin1)), equals(as.numeric(logLik(qbin1))))
    expect_that(sum(llcont(qbin2)), equals(as.numeric(logLik(qbin2))))

    ## gaussian
    gau1 <- glm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,
                data=gala, family=gaussian)
    gau2 <- glm(Species ~ Area + Elevation + Nearest, data=gala,
                family=gaussian)

    expect_that(sum(llcont(gau1)), equals(as.numeric(logLik(gau1))))
    expect_that(sum(llcont(gau2)), equals(as.numeric(logLik(gau2))))

    ## inverse.gaussian
    invGau1 <- glm(actual ~ projected-1,
                   family=inverse.gaussian(link="identity"), cpd)

    expect_that(sum(llcont(invGau1)), equals(as.numeric(logLik(invGau1))))

    ## Gamma
    clotting <- data.frame(u = c(5,10,15,20,30,40,60,80,100),
                           lot1 = c(118,58,42,35,27,25,21,19,18),
                           lot2 = c(69,35,26,21,18,16,13,12,12))
    gam1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)

    expect_that(sum(llcont(gam1)), equals(as.numeric(logLik(gam1))))

    ## poisson
    counts <- c(18,17,15,20,10,20,25,13,12)
    outcome <- gl(3,1,9)
    treatment <- gl(3,3)
    d.AD <- data.frame(treatment, outcome, counts)
    pois1 <- glm(counts ~ outcome + treatment, family = poisson)
    pois2 <- glm(counts ~ outcome, family = poisson)

    expect_that(sum(llcont(pois1)), equals(as.numeric(logLik(pois1))))
    expect_that(sum(llcont(pois2)), equals(as.numeric(logLik(pois2))))

    ## quasipoisson
    qpois1 <- glm(counts ~ outcome + treatment, family = quasipoisson)
    qpois2 <- glm(counts ~ outcome, family = quasipoisson)

    expect_that(sum(llcont(qpois1)), equals(as.numeric(logLik(qpois1))))
    expect_that(sum(llcont(qpois2)), equals(as.numeric(logLik(qpois2))))

    ## negative-binomial: MASS
    nb1 <- glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)

    expect_that(sum(llcont(nb1)), equals(as.numeric(logLik(nb1))))
  }
})


test_that("clm object", {
  if (isTRUE(require("ordinal")) && isTRUE(require("MASS"))) {
    clm1 <- clm(rating ~ temp * contact, data = wine)
    clm2 <- update(clm1, ~.-temp:contact)
    clm3 <- update(clm1, link = "logit")
    clm4 <- update(clm1, link = "probit")
    clm5 <- update(clm1, link = "loglog")
    clm6 <- update(clm1, link = "cloglog")
    clm7 <- update(clm1, link = "cauchit")
    clm8 <- update(clm1, threshold = "symmetric")
    clm9 <- update(clm1, threshold = "equidistant")
    clm10 <- clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

    expect_that(sum(llcont(clm1)), equals(as.numeric(logLik(clm1))))
    expect_that(sum(llcont(clm2)), equals(as.numeric(logLik(clm2))))
    expect_that(sum(llcont(clm3)), equals(as.numeric(logLik(clm3))))
    expect_that(sum(llcont(clm4)), equals(as.numeric(logLik(clm4))))
    expect_that(sum(llcont(clm5)), equals(as.numeric(logLik(clm5))))
    expect_that(sum(llcont(clm6)), equals(as.numeric(logLik(clm6))))
    expect_that(sum(llcont(clm7)), equals(as.numeric(logLik(clm7))))
    expect_that(sum(llcont(clm8)), equals(as.numeric(logLik(clm8))))
    expect_that(sum(llcont(clm9)), equals(as.numeric(logLik(clm9))))
    expect_that(sum(llcont(clm10)), equals(as.numeric(logLik(clm10))))
  }
})


test_that("hurdle object", {
  if (isTRUE(require("pscl"))) {
    hurdle1 <- hurdle(formula = art ~ ., data = bioChemists)
    hurdle2 <- hurdle(formula = art ~ ., data = bioChemists, separate=FALSE)
    hurdle3 <- hurdle(art ~ ., data = bioChemists, zero = "geometric")
    hurdle4 <- hurdle(art ~ fem + ment, data = bioChemists,
                      dist = "negbin", zero = "negbin")
    hurdle5 <- hurdle(art ~ ., data = bioChemists, dist = "negbin")

    expect_that(sum(llcont(hurdle1)), equals(as.numeric(logLik(hurdle1))))
    expect_that(sum(llcont(hurdle2)), equals(as.numeric(logLik(hurdle2))))
    expect_that(sum(llcont(hurdle3)), equals(as.numeric(logLik(hurdle3))))
    expect_that(sum(llcont(hurdle4)), equals(as.numeric(logLik(hurdle4))))
    expect_that(sum(llcont(hurdle5)), equals(as.numeric(logLik(hurdle5))))
  }
})


test_that("zeroinfl object", {
  if (isTRUE(require("pscl"))) {
    zi1 <- zeroinfl(art ~ . | 1, data = bioChemists)
    zi2 <- zeroinfl(art ~ . | 1, data = bioChemists, dist = "negbin")
    zi3 <- zeroinfl(art ~ . | ., data = bioChemists)
    zi4 <- zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")

    expect_that(sum(llcont(zi1)), equals(as.numeric(logLik(zi1))))
    expect_that(sum(llcont(zi2)), equals(as.numeric(logLik(zi2))))
    expect_that(sum(llcont(zi3)), equals(as.numeric(logLik(zi3))))
    expect_that(sum(llcont(zi4)), equals(as.numeric(logLik(zi4))))
  }
})


test_that("lm object", {
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  lm1 <- lm(weight ~ group)

  expect_that(sum(llcont(lm1)), equals(as.numeric(logLik(lm1))))
})


test_that("mlogit object", {
  if (isTRUE(require("mlogit"))) {
    data("Fishing", package = "mlogit")
    Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide",
                        choice = "mode")
    mlog1 <- mlogit(mode ~ price + catch, data = Fish)
    mlog2 <- mlogit(mode ~ 0 | income, data = Fish)
    mlog3 <- mlogit(mode ~ price+ catch | income, data = Fish)
    mlog4 <- mlogit(mode ~ price+ catch | income, data = Fish,
                    reflevel = "charter")
    mlog5 <- mlogit(mode ~ price+ catch | income, data = Fish,
                    alt.subset = c("charter", "pier", "beach"))
    Fishing2 <- Fishing
    Fishing2[1, "price.pier"] <- Fishing2[3, "price.beach"] <- NA
    mlog6 <- mlogit(mode~price+catch|income, Fishing2, shape="wide",
                    choice="mode", varying = 2:9)
    data("TravelMode", package = "AER")
    Tr2 <- TravelMode[-c(2, 7, 9),]
    mlog7 <- mlogit(choice~wait+gcost|income+size, Tr2, shape = "long",
                    chid.var = "individual", alt.var="mode", choice = "choice")
    data("TravelMode", package = "AER")
    mlog8 <- mlogit(choice ~ wait + travel + vcost, TravelMode,
                    shape = "long", chid.var = "individual",
                    alt.var = "mode",
                    method = "bfgs", heterosc = TRUE, tol = 10)
    TravelMode$avincome <- with(TravelMode, income * (mode == "air"))
    TravelMode$time <- with(TravelMode, travel + wait)/60
    TravelMode$timeair <- with(TravelMode, time * I(mode == "air"))
    TravelMode$income <- with(TravelMode, income / 10)
    TravelMode$incomeother <- with(TravelMode,
                                   ifelse(mode %in% c('air', 'car'),
                                          income, 0))
    mlog9 <- mlogit(choice~gcost+wait+incomeother, TravelMode,
                    shape='long', alt.var='mode',
                    nests=list(public=c('train', 'bus'),
                        other=c('car','air')))
    data("Game", package = "mlogit")
    mlog10 <- mlogit(ch~own|hours, Game, choice='ch', varying = 1:12,
                     ranked=TRUE, shape="wide", reflevel="PC")

    expect_that(sum(llcont(mlog1)), equals(as.numeric(logLik(mlog1))))
    expect_that(sum(llcont(mlog2)), equals(as.numeric(logLik(mlog2))))
    expect_that(sum(llcont(mlog3)), equals(as.numeric(logLik(mlog3))))
    expect_that(sum(llcont(mlog4)), equals(as.numeric(logLik(mlog4))))
    expect_that(sum(llcont(mlog5)), equals(as.numeric(logLik(mlog5))))
    expect_that(sum(llcont(mlog6)), equals(as.numeric(logLik(mlog6))))
    expect_that(sum(llcont(mlog7)), equals(as.numeric(logLik(mlog7))))
    expect_that(sum(llcont(mlog8)), equals(as.numeric(logLik(mlog8))))
    expect_that(sum(llcont(mlog9)), equals(as.numeric(logLik(mlog9))))
    expect_that(sum(llcont(mlog10)), equals(as.numeric(logLik(mlog10))))
  }
})


test_that("nls object", {
  DNase1 <- subset(DNase, Run == 1)
  nls1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
  nls2 <- nls(density ~ 1/(1 + exp((xmid - log(conc))/scal)),
              data = DNase1,
              start = list(xmid = 0, scal = 1),
              algorithm = "plinear")
  nls3 <- nls(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
              data = DNase1,
              start = list(Asym = 3, xmid = 0, scal = 1))
  nls4 <- nls(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
              data = DNase1,
              start = list(Asym = 3, xmid = 0, scal = 1),
              algorithm = "port")
  Treated <- Puromycin[Puromycin$state == "treated", ]
  weighted.MM <- function(resp, conc, Vm, K) {
    pred <- (Vm * conc)/(K + conc)
    (resp - pred) / sqrt(pred)
  }
  nls5 <- nls( ~ weighted.MM(rate, conc, Vm, K), data = Treated,
              start = list(Vm = 200, K = 0.1))
  lisTreat <- with(Treated,
                   list(conc1 = conc[1], conc.1 = conc[-1], rate = rate))
  weighted.MM1 <- function(resp, conc1, conc.1, Vm, K) {
    conc <- c(conc1, conc.1)
    pred <- (Vm * conc)/(K + conc)
    (resp - pred) / sqrt(pred)
  }
  nls6 <- nls( ~ weighted.MM1(rate, conc1, conc.1, Vm, K),
              data = lisTreat, start = list(Vm = 200, K = 0.1))
  weighted.MM.grad <- function(resp, conc1, conc.1, Vm, K) {
    conc <- c(conc1, conc.1)
    K.conc <- K+conc
    dy.dV <- conc/K.conc
    dy.dK <- -Vm*dy.dV/K.conc
    pred <- Vm*dy.dV
    pred.5 <- sqrt(pred)
    dev <- (resp - pred) / pred.5
    Ddev <- -0.5*(resp+pred)/(pred.5*pred)
    attr(dev, "gradient") <- Ddev * cbind(Vm = dy.dV, K = dy.dK)
    dev
  }
  nls7 <- nls( ~ weighted.MM.grad(rate, conc1, conc.1, Vm, K),
              data = lisTreat, start = list(Vm = 200, K = 0.1))
  x <- -(1:100)/10
  y <- 100 + 10 * exp(x / 2) + rnorm(x)/10
  nls8 <- suppressWarnings(nls(y ~  Const + A * exp(B * x)))
  utils::data(muscle, package = "MASS")
  nls9 <- nls(Length ~ cbind(1, exp(-Conc/th)), muscle,
              start = list(th = 1), algorithm = "plinear")
  b <- coef(nls9)
  nls10 <- nls(Length ~ a[Strip] + b[Strip]*exp(-Conc/th), muscle,
               start = list(a = rep(b[2], 21), b = rep(b[3], 21),
                   th = b[1]))

  expect_that(sum(llcont(nls1)), equals(as.numeric(logLik(nls1))))
  expect_that(sum(llcont(nls2)), equals(as.numeric(logLik(nls2))))
  expect_that(sum(llcont(nls3)), equals(as.numeric(logLik(nls3))))
  expect_that(sum(llcont(nls4)), equals(as.numeric(logLik(nls4))))
  expect_that(sum(llcont(nls5)), equals(as.numeric(logLik(nls5))))
  expect_that(sum(llcont(nls6)), equals(as.numeric(logLik(nls6))))
  expect_that(sum(llcont(nls7)), equals(as.numeric(logLik(nls7))))
  expect_that(sum(llcont(nls8)), equals(as.numeric(logLik(nls8))))
  expect_that(sum(llcont(nls9)), equals(as.numeric(logLik(nls9))))
  expect_that(sum(llcont(nls10)), equals(as.numeric(logLik(nls10))))
})


test_that("polr object", {
  if (isTRUE(require("MASS"))) {
    options(contrasts = c("contr.treatment", "contr.poly"))
    polr1 <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
    polr2 <- update(polr1, method = "probit", Hess = TRUE)
    polr3 <- update(polr1, method = "loglog", Hess = TRUE)
    polr4 <- update(polr1, method = "cloglog", Hess = TRUE)

    expect_that(sum(llcont(polr1)), equals(as.numeric(logLik(polr1))))
    expect_that(sum(llcont(polr2)), equals(as.numeric(logLik(polr2))))
    expect_that(sum(llcont(polr3)), equals(as.numeric(logLik(polr3))))
    expect_that(sum(llcont(polr4)), equals(as.numeric(logLik(polr4))))
  }
})


test_that("rlm object", {
  if (isTRUE(require("MASS"))) {
    rlm1 <- rlm(stack.loss ~ ., stackloss)
    rlm2 <- rlm(stack.loss ~ ., stackloss, psi = psi.hampel, init = "lts")
    rlm3 <- rlm(stack.loss ~ ., stackloss, psi = psi.bisquare)

    expect_that(sum(llcont(rlm1)), equals(as.numeric(logLik(rlm1))))
    expect_that(sum(llcont(rlm2)), equals(as.numeric(logLik(rlm2))))
    expect_that(sum(llcont(rlm3)), equals(as.numeric(logLik(rlm3))))
  }
})