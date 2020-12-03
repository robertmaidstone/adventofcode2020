read.csv("input_1.txt") -> inputnumbers

library(tidyverse)

tibble(input=inputnumbers[[1]]) %>%
  mutate(need = 2020 - input) %>%
  mutate(output = input * need) %>%
  mutate(answer = need %in% input) %>%
  filter(answer)


####################

outer(inputnumbers[[1]],inputnumbers[[1]],"+") %>% as.numeric -> pair_sums


tibble(input=inputnumbers[[1]]) %>%
  mutate(need = 2020 - input) %>%
  #mutate(output = input * need) %>%
  mutate(answer = need %in% pair_sums) %>%
  filter(answer) %>% 
  dplyr::select(input) %>% prod
