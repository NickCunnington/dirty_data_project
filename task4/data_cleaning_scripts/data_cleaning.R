library(tidyverse)
library(readxl)
library(janitor)

candy_2015 <- readxl::read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- readxl::read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- readxl::read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")

#clean row names
candy_2015_named <- clean_names(candy_2015)
candy_2016_named <- clean_names(candy_2016)
candy_2017_named <- clean_names(candy_2017)

#turn everything into lowercase
candy_2015_lower <-  mutate_if(candy_2015_named, is.character, str_to_lower)
candy_2016_lower <-  mutate_if(candy_2016_named, is.character, str_to_lower)
candy_2017_lower <-  mutate_if(candy_2017_named, is.character, str_to_lower)

#remove columns that are not required
candy_2015_shortened <- candy_2015_lower %>%
  select(-please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_beyonce,
         -please_leave_any_remarks_or_comments_regarding_your_choices,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_jk_rowling,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_jj_abrams,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_beyonce,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_bieber,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_kevin_bacon,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_francis_bacon_1561_1626,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_bruce_lee,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_jk_rowling,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_malala_yousafzai,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_thom_yorke,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_jj_abrams,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_hillary_clinton,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_donald_trump,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_beyonce_knowles,
         -check_all_that_apply_i_cried_tears_of_sadness_at_the_end_of,
         -that_dress_that_went_viral_early_this_year_when_i_first_saw_it_it_was,
         -what_is_your_favourite_font,
         -if_you_squint_really_hard_the_words_intelligent_design_would_look_like,
         -fill_in_the_blank_taylor_swift_is_a_force_for,
         -fill_in_the_blank_imitation_is_a_form_of,
         -which_day_do_you_prefer_friday_or_sunday,
         -guess_the_number_of_mints_in_my_hand,
         -betty_or_veronica,
         -please_list_any_items_not_included_above_that_give_you_joy,
         -please_list_any_items_not_included_above_that_give_you_despair)


#remove columns that are not required
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
         -york_peppermint_patties_ignore,
         -please_list_any_items_not_included_above_that_give_you_joy,
         -please_list_any_items_not_included_above_that_give_you_despair)


#remove columns that are not required
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
         -click_coordinates_x_y,
         -q7_joy_other,
         -q8_despair_other,
         -q2_gender,
         -internal_id,
         -q5_state_province_county_etc)

rm(candy_2015)
rm(candy_2015_lower)
rm(candy_2015_named)
rm(candy_2016)
rm(candy_2016_lower)
rm(candy_2016_named)
rm(candy_2017)
rm(candy_2017_lower)
rm(candy_2017_named)

#move 2017 timestamp to column 1
candy_2017_shortened <- candy_2017_shortened[,c(105, 1:104)]

#format timestamps to be years only
candy_2015_shortened$timestamp <- lubridate::year(candy_2015_shortened$timestamp)
candy_2016_shortened$timestamp <- lubridate::year(candy_2016_shortened$timestamp)
candy_2017_shortened$timestamp <- rep("2017-01-01", times = 2460)
candy_2017_shortened$timestamp <- lubridate::as_date(candy_2017_shortened$timestamp)
candy_2017_shortened$timestamp <- lubridate::year(candy_2017_shortened$timestamp)


#candy_2015_shortened$how_old_are_you[which(candy_2015_shortened$how_old_are_you > 120)] <- NA
#candy_2015_shortened$how_old_are_you[which(candy_2015_shortened$how_old_are_you < 5)] <- NA

#candy_2015_shortened$how_old_are_you <- as.double(candy_2015_shortened$how_old_are_you)
#candy_2016_shortened$how_old_are_you <- as.double(candy_2016_shortened$how_old_are_you)
#candy_2017_shortened$q3_age <- as.double(candy_2017_shortened$q3_age)

#candy_all <- bind_rows(candy_2015_shortened, candy_2016_shortened, candy_2017_shortened)

us_vector <- c("united states of america", "the yoo ess of aaayyyyyy", "united states",
               "united states", "united state", "the best one - usa", "the best one - usa",
               "units states", "america", "merica", "murica", "usa!!!!!!", "usa usa usa usa",
               "usa! usa! usa!", "usa usa usa", "usa!", "ussa", "u.s.a", "us")


candy_2016_country <- candy_2016_shortened %>%
  mutate(country = ifelse(str_detect(which_country_do_you_live_in, str_c(us_vector, collapse = "|")),
                          "usa", which_country_do_you_live_in))











candy_2016_country <- candy_2016_shortened %>%
  mutate(ifelse(str_detect(which_country_do_you_live_in,"united states of america", "usa")) %>%
                  ifelse(str_detect("the yoo ess of aaayyyyyy", "usa")) %>% 
                  ifelse(str_detect("united states", "usa")) %>%
                  ifelse(str_detect("united state", "usa")) %>%
                  ifelse(str_detect("the best one - usa", "usa")) %>%
                  ifelse(str_detect("units states", "usa")) %>%
                  ifelse(str_detect("america", "usa")) %>%
                  ifelse(str_detect("merica", "usa")) %>%
                  ifelse(str_detect("murica", "usa")) %>%
                  ifelse(str_detect("usa!!!!!!", "usa")) %>%
                  ifelse(str_detect("usa usa usa usa", "usa")) %>%
                  ifelse(str_detect("usa! usa! usa!", "usa")) %>%
                  ifelse(str_detect("usa usa usa", "usa")) %>%
                  ifelse(str_detect("usa!", "usa")) %>%
                  ifelse(str_detect("ussa", "usa")) %>%
                  ifelse(str_detect("u.s.a", "usa")) %>%
                  ifelse(str_detect("us", "usa")))
 

  
  

candy_2016_country <- candy_2016_shortened %>%
  mutate(which_country_do_you_live_in =
           str_detect(which_country_do_you_live_in, us_vector))
  
  
         
         
         
         
         
         
         
  
  

unique(candy_2016_country$which_country_do_you_live_in)


