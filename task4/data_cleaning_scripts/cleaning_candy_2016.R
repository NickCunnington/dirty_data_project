library(tidyverse)
library(readxl)
library(janitor)

candy_2016 <- readxl::read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")

#change column names to change fullstops to underscores
candy_2016_named <- clean_names(candy_2016)

#turns everything into lower case
candy_2016_lower <-  mutate_if(candy_2016_named, is.character, str_to_lower)

#changing age column from chr to num
candy_2015_lower$how_old_are_you <- as.double(candy_2015_named$how_old_are_you)

#getting rid of extreems of values of ages.  Anything greater then 120 years and 
#lower than 5 years old will be turned into NA
candy_2016_lower$how_old_are_you[which(candy_2016_lower$how_old_are_you > 120)] <- NA
candy_2016_lower$how_old_are_you[which(candy_2016_lower$how_old_are_you < 5)] <- NA

candy_2016_shortened <- candy_2016_lower %>%
  select(-person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes,
         -please_leave_any_witty_snarky_or_thoughtful_remarks_or_comments_regarding_your_choices,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_jk_rowling,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_jj_abrams,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_beyonce,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_bieber,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_kevin_bacon,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_francis_bacon_1561_1626,
         -guess_the_number_of_mints_in_my_hand,
         -betty_or_veronica,
         -that_dress_that_went_viral_a_few_years_back_when_i_first_saw_it_it_was,
         -what_is_your_favourite_font,
         -which_day_do_you_prefer_friday_or_sunday,
         -do_you_eat_apples_the_correct_way_east_to_west_side_to_side_or_do_you_eat_them_like_a_freak_of_nature_south_to_north_bottom_to_top,
         -when_you_see_the_above_image_of_the_4_different_websites_which_one_would_you_most_likely_check_out_please_be_honest,
         -york_peppermint_patties_ignore)
