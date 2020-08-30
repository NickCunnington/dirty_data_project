library(tidyverse)
library(janitor)

decathlon <- readRDS("raw_data/decathlon.rds")

# clean column names
decathlon_cleannames <- clean_names(decathlon)

# rename columns, change format and pivot to long format
decathlon_clean <- decathlon_cleannames %>%
  rownames_to_column("surname") %>%
  rename(running_100m = x100m, running_400m = x400m, hurdle_110m = x110m_hurdle,
         running_1500m = x1500m, javelin = javeline) %>%
  mutate(surname = tolower(surname), competition = as.character(competition)) %>%
  pivot_longer(running_100m:running_1500m, names_to = "event", values_to = "score")

# write to csv
write_csv(decathlon_clean, "clean_data/decathlon_clean.csv")