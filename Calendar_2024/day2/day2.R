library(dplyr)

requirements_for_valid <- function(row) {
  diffs <- diff(row)
  (all(diffs > 0) || all(diffs < 0)) && all(abs(diffs) >= 1 & abs(diffs) <= 3)
}

valid_count <- readLines("data.txt") %>%
  lapply(function(line) as.numeric(strsplit(line, " ")[[1]])) %>%
  vapply(requirements_for_valid, logical(1)) %>%
  sum()

valid_count
