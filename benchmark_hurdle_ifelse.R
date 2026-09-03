# Reproducible benchmark harness
# Install the optional benchmark dependency with:
# install.packages("microbenchmark")
library(microbenchmark)

run_orig <- function(y, wt) {
  n <- rowSums(y)
  y_orig <- ifelse(n == 0, 0, y[, 1]/n)
  m <- if (any(n > 1)) n else wt
  wt_orig <- ifelse(m > 0, (wt/m), 0)
  list(y = y_orig, wt = wt_orig)
}

run_opt <- function(y, wt) {
  n <- rowSums(y)
  y_opt <- unname(y[, 1]/n)
  y_opt[n == 0] <- 0
  m <- if (any(n > 1)) n else wt
  if (length(wt) == 1 && length(m) > 1) wt <- rep(wt, length(m))
  res_wt <- unname(wt/m)
  res_wt[m <= 0] <- 0
  wt_opt <- res_wt
  list(y = y_opt, wt = wt_opt)
}

set.seed(20260811)
n <- 1000000
y <- matrix(rbinom(n * 2, 1, 0.5), nrow=n, ncol=2)
wt <- 1

bm <- microbenchmark(
  original = run_orig(y, wt),
  optimized = run_opt(y, wt),
  times = 100,
  control = list(warmup = 10)
)
print(bm)

med_orig <- median(bm$time[bm$expr == "original"])
med_opt <- median(bm$time[bm$expr == "optimized"])
improvement <- (med_orig - med_opt) / med_orig

cat(sprintf("Performance improvement: %.2f%%\n", improvement * 100))
