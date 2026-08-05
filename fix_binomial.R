lines <- readLines("R/llcont.R")
start_idx <- grep("if\\(is\\.matrix\\(y\\)\\)", lines)[1]
end_idx <- grep("} else \\{", lines)[1]

new_lines <- c(
  "             if(is.matrix(y)) {",
  "               ## Bolt: replaced apply(..., 1, sum) with optimized rowSums() for performance",
  "               n <- rowSums(y)",
  "               ## Bolt: replaced ifelse with vectorized subsetting for performance",
  "               y_res <- n * 0",
  "               n_cond <- n == 0",
  "               if (any(n_cond)) y_res[n_cond] <- 0",
  "               if (any(!n_cond)) y_res[!n_cond] <- y[, 1][!n_cond] / n[!n_cond]",
  "               y <- y_res"
)

lines <- c(lines[1:(start_idx - 1)], new_lines, lines[end_idx:length(lines)])
writeLines(lines, "R/llcont.R")
