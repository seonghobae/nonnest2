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
## 2024-11-20 - R ifelse() Performance Optimization
**Learning:** In R, `ifelse(cond, yes, no)` evaluates both the true and false branches entirely before subsetting, which causes major performance bottlenecks for expensive vectorized operations (like log or exp on large likelihood vectors). Furthermore, directly replacing it with vectorized subsetting (e.g., `res[cond] <- weights[cond] * ...`) can trigger vector recycling bugs if scalars (like `weights`) are not explicitly padded/recycled to match the condition length.
**Action:** Replace `ifelse()` overhead in hot loops with explicit preallocation that preserves structure (`res <- Y * 0`) and manually expanded scalars (`wt_adj <- if(length(weights) == 1) rep_len(weights, length(Y)) else weights`) before performing condition-based subset assignment.
