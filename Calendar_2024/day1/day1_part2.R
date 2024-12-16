# pipe it up DW :D
library(dplyr)

setwd("~/Desktop/advent_of_code/Calendar_2024/day1")
data <- read.table("data.txt", header = FALSE)

# 0(n) solution
counts <- data %>%
  count(V2, name = "count")
ans <- data %>%
  left_join(counts, by = c("V1" = "V2")) %>%
  mutate(score = V1 * count) %>%
  summarise(total_score = sum(score, na.rm = TRUE)) %>%
  pull(total_score)

print(ans)