library(tidyverse)


cake <- read_csv("raw_data/cake-ingredients-1961.csv")
names <- read_csv("raw_data/cake_ingredient_code.csv")

# pivoting long then join on names, dropping any NAs and trimming white space
cake_clean <-
  cake %>%
  pivot_longer(-Cake, names_to = "code", values_to = "amount") %>%
  rename(cake = Cake) %>%
  left_join(names) %>%
  drop_na(amount) %>%
  mutate(cake = str_trim(cake)) %>%
  select(-code)

write_csv(cake_clean, "clean_data/cake_clean.csv")