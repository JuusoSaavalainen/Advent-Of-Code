library(dplyr)

requirements_for_valid <- function(row) {
  diffs <- diff(row)
  (all(diffs > 0) || all(diffs < 0)) && all(abs(diffs) >= 1 & abs(diffs) <= 3)
}

can_be_valid_if_one_removed <- function(row) {
  any(sapply(seq_along(row), function(i) requirements_for_valid(row[-i])))
}

total_count <- readLines("data.txt") %>%
  lapply(function(line) as.numeric(strsplit(line, " ")[[1]])) %>%
  vapply(function(row) requirements_for_valid(row) || can_be_valid_if_one_removed(row), logical(1)) %>%
  sum()

total_count
