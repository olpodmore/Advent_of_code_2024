setwd("C:/Users/op000006/OneDrive - Defra/L&D/AoC_2024")

library(readr)
library(dplyr)

input <- read_delim("input01.txt", col_names = FALSE, delim = "   ")

input_X1 <- input %>%
  select(X1) %>%
  arrange(X1)

input_X2 <- input %>%
  select(X2) %>%
  arrange(X2)

input_sorted <- tibble(input_X1, input_X2) %>%
  mutate(difference = abs(X1 - X2))

# answer to part 1
sum(input_sorted$difference)


# part 2 ------------------------------------------------------------------

input_similarity <- input_sorted %>%
  mutate(check = X1 %in% X2)

numbers_in_right_list <- input_similarity %>%
  filter(check == TRUE) %>%
  select(X1)

right_list <- input_similarity %>%
  filter(X2 %in% numbers_in_right_list$X1) %>%
  select(X2) %>%
  group_by(X2) %>%
  summarise(count=n()) %>%
  mutate(similarity_score = X2 * count)

# answer to part 2
sum(right_list$similarity_score)
