# ohh hell naww might switch to python this was ... pain 
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
check_rules <- function(input, rules) {
  vapply(rules, function(rule) {
    order_diff <- diff(match(rule, input))
    is.na(order_diff) || all(order_diff > 0)
  }, logical(1)) %>%
    all()
}
filter_valid_inputs <- function(inputs, rules) {
  vapply(inputs, check_rules, logical(1), rules)
}
get_middle <- function(x) {
  x[ceiling(length(x) / 2)]
}
result <- function(inputs, rules) {
  valid_inputs <- filter_valid_inputs(inputs, rules)
  inputs[valid_inputs] %>%
    lapply(get_middle) %>%
    unlist() %>%
    sum()
}
data <- parse_data(lines, separator)
final_result <- result(data$inputs, data$rules)
print(final_result)