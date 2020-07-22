library(tidyverse)
library(readxl)
library(janitor)

candy_2017 <- readxl::read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")

#change column names to change fullstops to underscores
candy_2017_named <- clean_names(candy_2017)

#turns everything into lower case
candy_2017_lower <-  mutate_if(candy_2017_named, is.character, str_to_lower)

#changing age column from chr to num
candy_2017_lower$q3_age <- as.double(candy_2017_named$q3_age)

#getting rid of extremes of values of ages.  Anything greater then 120 years and 
#lower than 5 years old will be turned into NA
candy_2017_lower$q3_age[which(candy_2017_lower$q3_age > 120)] <- NA
candy_2017_lower$q3_age[which(candy_2017_lower$q3_age < 5)] <- NA

#there are several extra column that are not required for this analysis
candy_2017_shortened <- candy_2017_lower %>%
  select(-q6_real_housewives_of_orange_county_season_9_blue_ray,
         -q6_cash_or_other_forms_of_legal_tender,
         -q9_other_comments,
         -q10_dress,
         -x114,
         -q11_day,
         -q12_media_daily_dish,
         -q12_media_science,
         -q12_media_espn,
         -q12_media_yahoo,
         -click_coordinates_x_y)

#cleaning country data
