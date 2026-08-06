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

## 2024-08-05 - Avoid ifelse for expensive operations in R
**Learning:** In R, `ifelse()` evaluates both true and false branches entirely before subsetting, which is inefficient for expensive operations and can cause mathematical hazards (e.g. `log(0)`).
**Action:** Optimize this overhead by replacing `ifelse()` with preallocation that preserves attributes (e.g., `res <- Y * 0`) and vectorized subsetting.

## 2024-08-05 - Rejected R-only Optimization for Statistical Likelihoods
**Learning:** Replacing `ifelse` with indexed assignment in active statistical likelihood arithmetic requires numerical and scientific evidence. `ifelse` propagates `NA`, while `if (any(Y1))` can error on `NA`. Mathematical layers require a documented Rust f64 kernel with bounded CPU multithreading and GPU path, while one simple R implementation remains an independent parity oracle.
**Action:** Avoid R-only optimizations in statistical likelihood layers. Propose a clean Rust/R vertical slice covering stable `log1mexp`, explicit semantics, true-parameter simulations, parity tests, benchmark, 100% coverage, CHANGELOG, ADR, and documentation instead.
