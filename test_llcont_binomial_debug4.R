source("R/llcont.R")
set.seed(42)
N <- 50
y_succ <- rbinom(N, 10, 0.5)
y_fail <- 10 - y_succ
y_succ[c(1, 15, 30)] <- 0
y_fail[c(1, 15, 30)] <- 0
x_pred <- runif(N)
wts <- rep(1, N)
wts[10:20] <- 0
wts[25:27] <- 0.5
df_bin <- data.frame(y_succ, y_fail, x_pred, wts)

bin_fit <- glm(cbind(y_succ, y_fail) ~ x_pred, family = binomial, data = df_bin, weights = wts)
cat("head(model.response(model.frame(bin_fit))):\n")
print(head(model.response(model.frame(bin_fit))))
cat("class of that: ", class(model.response(model.frame(bin_fit))), "\n")
