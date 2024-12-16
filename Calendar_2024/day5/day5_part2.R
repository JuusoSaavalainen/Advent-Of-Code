# naaahhhh
library(dplyr)

setwd("~/Desktop/advent_of_code/Calendar_2024/day5")
lines <- readLines("data.txt")
separator <- which(lines == "")

parse_data <- function(lines, separator) {
  part1 <- lines[1:(separator - 1)]
  part2 <- lines[(separator + 1):length(lines)]
  list(
    rules = parse_rules(part1),
    inputs = parse_inputs(part2)
  )
}
parse_rules <- function(rule_lines) {
  rule_lines %>%
    strsplit("\\|") %>%
    lapply(function(x) as.numeric(trimws(x)))
}
parse_inputs <- function(input_lines) {
  input_lines %>%
    lapply(function(x) {
      strsplit(x, ",") %>%
        unlist() %>%
        as.numeric()
    })
}
pass_one <- function(rule, update) {
  order <- diff(match(rule, update))
  is.na(order) || order > 0
}
pass_all <- function(update, rules) {
  all(vapply(rules, pass_one, logical(1), update))
}
fix_order <- function(rule, update) {
  new_order <- update
  for (i in 1:(length(rule) - 1)) {
    pos1 <- match(rule[i], update)
    pos2 <- match(rule[i + 1], update)
    if (pos1 > pos2) {
      new_order[c(pos1, pos2)] <- new_order[c(pos2, pos1)]
    }
  }
  return(new_order)
}
fix_update <- function(update, rules) {
  if (pass_rules(update, rules)) {
    return(update)
  }
  for (rule in rules) {
    if (!pass_one(rule, update)) {
      update <- fix_order(rule, update)
      update <- fix_update(update, rules)
    }
  }
  return(update)
}
get_middle <- function(x) {
  x[ceiling(length(x) / 2)]
}
data <- parse_data(lines, separator)
passs <- vapply(data$inputs, pass_all, logical(1), rules = data$rules)
uncorrect <- data$inputs[!passs]
sum_middle_values_invalid <- uncorrect %>%
  lapply(fix_update, rules = data$rules) %>%
  lapply(get_middle) %>%
  unlist() %>%
  sum()
print(sum_middle_values_invalid)
