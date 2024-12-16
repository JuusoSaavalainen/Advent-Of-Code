library(stringr)
library(dplyr)

re_pattern <- "mul\\((\\d{1,3}),(\\d{1,3})\\)"
total_sum <- readLines("data.txt") %>%
  str_extract_all(re_pattern) %>%
  unlist() %>%
  str_match(re_pattern) %>%
  as.data.frame() %>%
  mutate(
    x = as.numeric(V2),
    y = as.numeric(V3)) %>%
  transmute(prod = x * y) %>%
  summarise(ans = sum(prod))
total_sum$ans

