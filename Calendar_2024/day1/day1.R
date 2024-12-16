# pipe it up DW :D
library(dplyr)

setwd("~/Desktop/advent_of_code/Calendar_2024/day1")
data <- read.table("data.txt", header = FALSE)

# n log n solution
ans <- data %>%
  mutate(V1 = sort(V1), V2 = sort(V2)) %>%
  mutate(V3 = abs(V2 - V1)) %>%
  summarise(sum_abs_diff = sum(V3)) %>%
  pull(sum_abs_diff)

print(ans)
