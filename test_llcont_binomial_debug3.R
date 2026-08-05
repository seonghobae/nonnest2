# the problem is that bin_fit$y is the actual response that glm uses internally which is proportion,
# but the code in llcont checks if is.matrix(y) and gets bin_fit$y, which is NOT A MATRIX anymore for binomial!
# WAIT. Let's check what x$y is.
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
cat("is.matrix(bin_fit$y): ", is.matrix(bin_fit$y), "\n")
cat("head(bin_fit$y): ", head(bin_fit$y), "\n")
cat("is.matrix(model.response(model.frame(bin_fit))): ", is.matrix(model.response(model.frame(bin_fit))), "\n")
