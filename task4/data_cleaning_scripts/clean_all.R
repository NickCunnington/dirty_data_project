library(tidyverse)
library(readxl)
library(janitor)

#read in xlsx files
candy_2015 <- readxl::read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- readxl::read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- readxl::read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")


#clean up row names ----
candy_2015_named <- clean_names(candy_2015)
candy_2016_named <- clean_names(candy_2016)
candy_2017_named <- clean_names(candy_2017)


#turn everything into lower case ----
candy_2015_lower <-  mutate_if(candy_2015_named, is.character, str_to_lower)
candy_2016_lower <-  mutate_if(candy_2016_named, is.character, str_to_lower)
candy_2017_lower <-  mutate_if(candy_2017_named, is.character, str_to_lower)


#remove columns that are not required ----
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
         -please_list_any_items_not_included_above_that_give_you_despair,
         -hugs_actual_physical_hugs,
         -generic_brand_acetaminophen,
         -broken_glow_stick,
         -glow_sticks,
         -dental_paraphenalia,
         -cash_or_other_forms_of_legal_tender,
         -white_bread,
         -vicodin,
         -peterson_brand_sidewalk_chalk,
         -creepy_religious_comics_chick_tracts,
         -kale_smoothie,
         -whole_wheat_anything,
         -lapel_pins)

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
         -please_list_any_items_not_included_above_that_give_you_despair,
         -whole_wheat_anything,
         -white_bread,
         -vicodin,
         -hugs_actual_physical_hugs,
         -glow_sticks,
         -generic_brand_acetaminophen,
         -creepy_religious_comics_chick_tracts,
         -chardonnay,
         -cash_or_other_forms_of_legal_tender,
         -broken_glow_stick,
         -bonkers_the_board_game,
         -which_state_province_county_do_you_live_in,
         -dental_paraphenalia,
         -kale_smoothie)      

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
         -internal_id,
         -q5_state_province_county_etc,
         -q6_white_bread,
         -q6_vicodin,
         -q6_hugs_actual_physical_hugs,
         -q6_glow_sticks,
         -q6_broken_glow_stick,
         -q6_dental_paraphenalia,
         -q6_chardonnay,
         -q6_whole_wheat_anything)


#set years ----
candy_2015_shortened$timestamp = "2015"
candy_2016_shortened$timestamp = "2016"
candy_2017_shortened$timestamp = "2017"


#enter in a columns for country and gender in 2015 ----
candy_2015_shortened$which_country_do_you_live_in = NA
candy_2015_shortened$your_gender = NA


#order 2015 data ----
candy_2015_ordered <- candy_2015_shortened[,c(1, 2, 3, 87, 86, 4:85)]
colnames(candy_2015_ordered)[1] <- "year"
colnames(candy_2015_ordered)[2] <- "age"
colnames(candy_2015_ordered)[3] <- "trick_or_treating"
colnames(candy_2015_ordered)[4] <- "country"
colnames(candy_2015_ordered)[5] <- "gender"
candy_2015_ordered <- candy_2015_ordered %>%
  mutate(gender = as.character(gender)) %>%
  mutate(gender = as.character(country))


#order 2016 data ----
candy_2016_ordered <- candy_2016_shortened[,c(1, 4, 2, 5, 3, 6:91)]
colnames(candy_2016_ordered)[1] <- "year"
colnames(candy_2016_ordered)[2] <- "age"
colnames(candy_2016_ordered)[3] <- "trick_or_treating"
colnames(candy_2016_ordered)[4] <- "country"
colnames(candy_2016_ordered)[5] <- "gender"


#order and rename 2017 data ----
candy_2017_ordered <- candy_2017_shortened[,c(98, 3, 1, 4, 2, 5:97)]
names(candy_2017_ordered) <- substring(names(candy_2017_ordered), 4)
colnames(candy_2017_ordered)[1] <- "year"
colnames(candy_2017_ordered)[2] <- "age"
colnames(candy_2017_ordered)[3] <- "trick_or_treating"
colnames(candy_2017_ordered)[4] <- "country"
colnames(candy_2017_ordered)[5] <- "gender"
colnames(candy_2017_ordered)[6] <- "x100_grand_bar"


#pivoting tables ----
candy_2015_pivoted <- candy_2015_ordered %>%
  pivot_longer(cols = 6:87,
               names_to = "candy_type",
               values_to = "rating")


candy_2016_pivoted <- candy_2016_ordered %>%
  pivot_longer(cols = 6:91,
               names_to = "candy_type",
               values_to = "rating")


candy_2017_pivoted <- candy_2017_ordered %>%
  pivot_longer(cols = 6:98,
               names_to = "candy_type",
               values_to = "rating")


#remove old objects from global environment ----
rm(candy_2015)
rm(candy_2015_lower)
rm(candy_2015_named)
rm(candy_2015_shortened)
rm(candy_2015_ordered)
rm(candy_2016)
rm(candy_2016_lower)
rm(candy_2016_named)
rm(candy_2016_shortened)
rm(candy_2016_ordered)
rm(candy_2017)
rm(candy_2017_lower)
rm(candy_2017_named)
rm(candy_2017_shortened)
rm(candy_2017_ordered)


#join all years together ----
candy_merged <- bind_rows(candy_2015_pivoted, candy_2016_pivoted, candy_2017_pivoted)


#format age column and filter >5 and <100 ----
candy_merged <- candy_merged %>%
  mutate(age = as.double(age)) %>%
  mutate(age = ifelse(age > 100, NA, age)) %>%
  mutate(age = ifelse(age < 5, NA, age))


#write to clean csv ----
write_csv(candy_merged, "clean_data/candy.csv")
rm(candy_2015_pivoted)
rm(candy_2016_pivoted)
rm(candy_2017_pivoted)
