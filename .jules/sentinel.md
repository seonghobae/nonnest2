## 2024-05-24 - Fix Information Disclosure in try()
**Vulnerability:** `try()` block in `llcont.R` defaulted to `silent = FALSE`, inadvertently leaking internal execution errors (e.g., matrix singularity details) to standard error.
**Learning:** R's `try()` defaults to printing errors unless `silent = TRUE` is explicitly provided.
**Prevention:** Always use `silent = TRUE` inside `try()` blocks or prefer `tryCatch()` to gracefully handle exceptions and prevent information disclosure.

## 2024-05-25 - Fix Information Disclosure in error handling
**Vulnerability:** Error handling in vuongtest used stop with the raw error object, which can expose internal execution details and call stacks.
**Learning:** Re-throwing errors directly propagates the entire condition object including the call, which can leak internal arguments and stack trace details.
**Prevention:** Always use stop with call. = FALSE and a custom generic message to prevent information disclosure.

## 2024-07-13 - Prevent Information Disclosure in all stop/warning calls
**Vulnerability:** Raw `stop()` and `warning()` calls without `call. = FALSE` in `llcont.R` and `vuongtest.R` exposed execution stack/call details when raised.
**Learning:** While some instances of `stop()` inside `tryCatch()` were previously fixed to hide the call stack, other standalone exceptions and warnings still leaked call context. Security must be consistently applied across the entire codebase.
**Prevention:** Always set `call. = FALSE` when using `stop()` or `warning()` to enforce a secure-by-default boundary and prevent internal execution paths from being disclosed to the end user.
## 2024-08-04 - Fix Information Disclosure and Logic Errors via Missing Input Validation
**Vulnerability:** Unvalidated arguments (`nested`, `adj`, `conf.level`) passed to exported functions `vuongtest()` and `icci()` could bypass top-level guards or cause `if()` length > 1 warnings/errors, leaking internal execution contexts.
**Learning:** In R codebases, unvalidated arguments passed to exported functions can bypass top-level safeguards and trigger raw R errors deep inside internal logic, leaking internal execution contexts.
**Prevention:** Always strictly validate the type, length, and bounds of user inputs at the very beginning of exported functions to fail securely and use `stop(..., call. = FALSE)` to prevent information disclosure.
