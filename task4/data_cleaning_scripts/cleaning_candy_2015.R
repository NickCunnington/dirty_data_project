library(tidyverse)
library(readxl)
library(janitor)

candy_2015 <- readxl::read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")

#change column names to change fullstops to underscores
candy_2015_named <- clean_names(candy_2015)

#turns everything into lower case
candy_2015_lower <-  mutate_if(candy_2015_named, is.character, str_to_lower)

#changing age column from chr to num
candy_2015_lower$how_old_are_you <- as.double(candy_2015_named$how_old_are_you)

#getting rid of extreems of values of ages.  Anything greater then 120 years and 
#lower than 5 years old will be turned into NA
candy_2015_lower$how_old_are_you[which(candy_2015_lower$how_old_are_you > 120)] <- NA
candy_2015_lower$how_old_are_you[which(candy_2015_lower$how_old_are_you < 5)] <- NA

#there are several extra column that are not required for this analysis
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
         -betty_or_veronica)

