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
## 2024-08-11 - Enforce secure error boundaries by validating input types early

**Vulnerability:**
Exported functions like `vuongtest` and `icci` expect certain arguments like `nested` to be logical scalars, and `adj` to be string scalars from a set of choices. When arrays or other types are passed (e.g. `nested=c(TRUE, FALSE)` or `adj=c("aic", "bic")`), the condition expressions (e.g., `if (nested)` or `if (adj == "aic")`) inside internal logic result in `length > 1` warnings/errors, or other raw R execution stack faults. These expose the internal execution flow to the user and violate the "fail securely" principle. They bypass the top-level `stop(..., call. = FALSE)` safeguards.

**Learning:**
In R, unvalidated arguments can bypass top-level input safeguards and trigger raw R errors deep inside internal logic, leaking internal execution contexts via error stack traces (e.g., `<simpleError in if (adj == "aic") ...>`). Relying on internal control flow `if` statements to implicitly validate inputs is insecure.

**Prevention:**
Always explicitly validate the type, length, and bounds of user inputs at the very beginning of exported functions (using assertions or functions like `match.arg()`) and use `stop(..., call. = FALSE)` to fail securely. For example:
```R
if (length(nested) > 1 || !is.logical(nested)) {
    stop("Argument 'nested' must be a single logical value.", call. = FALSE)
}
adj <- match.arg(adj, c("none", "aic", "bic"))
```
