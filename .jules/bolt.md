## 2024-07-25 - Vectorized Operations in R
**Learning:** In R codebases, using `apply(..., 1, sum)` or `apply(..., 2, mean)` on matrices is significantly slower than using the optimized, vectorized base equivalents `rowSums()` and `colMeans()`.
**Action:** Always prefer `rowSums()`, `colSums()`, `rowMeans()`, and `colMeans()` over `apply` for basic matrix summarization to ensure better performance.

## 2024-05-24 - Optimized Row-Wise String Concatenation in R
**Learning:** Using `apply(mat, 1, paste, collapse = "")` for row-wise string concatenation in R is very slow due to the loop overhead over rows in interpreted code.
**Action:** Always prefer `do.call(paste0, as.data.frame(mat))` to concatenate columns vectorized-style instead, which drastically speeds up the operation.

## 2024-05-25 - Avoid O(N^2) memory reallocation in R loops
**Learning:** Using `do.call(cbind, ...)` to grow an N-row object across K submodels causes $O(NK^2)$ cumulative copying and $O(NK)$ peak storage.
**Action:** Accumulate sums directly to keep $O(N)$ accumulator storage and $O(NK)$ total accumulation work.
## 2026-07-14 - Matrix Cross Product Optimization
**Learning:** In R, matrix multiplication of the form `t(X) %*% Y` explicitly allocates memory for the transposed matrix. Using the optimized base function `crossprod(X, Y)` avoids this allocation.
**Action:** Always replace `t(X) %*% Y` with `crossprod(X, Y)` for faster and more memory-efficient cross-product calculations.

## 2024-07-27 - Optimized ifelse with vectorized subsetting in R
**Learning:** `ifelse()` evaluates both true and false branches entirely before subsetting, causing inefficient memory allocation and overhead. When replacing it with vectorized subsetting (e.g., `res <- Y * 0; res[cond] <- ...`), scalar variables in the assigned expression must be explicitly recycled to match the condition vector's length (e.g., using `rep_len`) to prevent vector recycling bugs or `NA` values.
**Action:** Always prefer preallocation and vectorized subsetting over `ifelse()` for mathematical operations in R, and ensure scalar variables are safely recycled before logical subsetting.
