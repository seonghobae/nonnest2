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

cat("logLik: ", as.numeric(logLik(bin_fit)), "\n")
cat("llcont full function: ", sum(llcont(bin_fit)), "\n")

y_mat <- cbind(y_succ, y_fail)
n_mat <- rowSums(y_mat)

y_orig_mat <- ifelse(n_mat == 0, 0, y_mat[, 1]/n_mat)
y_res_mat <- n_mat * 0
n_cond_mat <- n_mat == 0
if (any(n_cond_mat)) y_res_mat[n_cond_mat] <- 0
if (any(!n_cond_mat)) y_res_mat[!n_cond_mat] <- y_mat[, 1][!n_cond_mat] / n_mat[!n_cond_mat]

cat("y_orig_mat: ", head(y_orig_mat), "\n")
cat("y_res_mat: ", head(y_res_mat), "\n")
print(all.equal(y_orig_mat, y_res_mat))
