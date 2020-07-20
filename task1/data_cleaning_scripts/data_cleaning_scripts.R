
library(tidyverse)
library(janitor)

#import data
raw_data <- read_rds("raw_data/decathlon.rds")

#change the row names to the first column
raw_data <- tibble::rownames_to_column(raw_data, "names")

#change first column to all lower case
raw_data$names <- tolower(raw_data$name)

#change column names to change fullstops to underscores
raw_data <- clean_names(raw_data)

#save clean data
write_csv(raw_data, "clean_data/clean_data.csv")

#convert table into long format
raw_data_long <- raw_data %>%
  pivot_longer(c(-names, -competition, -rank), names_to = "event", values_to = "result")

#saving clean data in long format
write_csv(raw_data_long, "clean_data/clean_data_long.csv")
