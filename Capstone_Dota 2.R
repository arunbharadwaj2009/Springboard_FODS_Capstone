library(tidyr)
library(dplyr)
library(ggplot2)

# Run exploratory analysis for match_csv to identify columns with missing values, unneccessary columns and outliers
hist(match_csv$game_mode)
hist(match_csv$negative_votes)
hist(match_csv$positive_votes)

# Game_mode, negative_votes and positive_votes are of only 1 type. So, these columns can be removed
match_csv$game_mode <- NULL
match_csv$negative_votes <- NULL
match_csv$positive_votes <- NULL

# Mean of first_blood_time column in match_csv returns 93. So, the first_blood_time column is denominated in seconds since 93 minutes is an impractical estimate of the average time to first blood. 
mean(match_csv$first_blood_time)

# 93 seconds for average first blood seems very short. It usually takes a player atleast 1.5 minutes to setup their heroes and move towards center of map.
sum(match_csv$first_blood_time < 100) # 29437 observations in first_blood_time were below 100 seconds. This indicates some error in the way first blood is calculated.

# In match_csv, cluster region is given as number. This is the only csv file that includes cluster. Instead of using numbers, join cluster_regions with match_csv
match_csv <- left_join(match_csv,cluster_regions_csv,by = "cluster")

# In chat_csv, some time values are negative. Check how many are negative
chat_negative <- (chat_csv$time < 0) # 77905 values are negative

# Convert all negative time values in chat table to 0 since most of the negative values lie at game start
chat_csv$time[chat_negative] <- 0

# Do something similar for the time column of purchase_log_csv table
purchase_log_negative <- (purchase_log_csv$time<0)
purchase_log_csv$time[purchase_log_negative] <- 0

# In players_csv, convert all 'None' observations in stuns column to 0
none_players_stuns <- which(players_csv$stuns == "None")
players_csv$stuns[none_players_stuns] <- 0 

# Write function to compute sum of NA`s in different columns of a dataset (Only choose columns that we believe have many NA`s)

players_csv %>% summarise_at(c("xp_other","xp_roshan","gold_killing_roshan","gold_buyback","unit_order_cast_toggle","unit_order_drop_item","unit_order_give_item","unit_order_disassemble_item", "unit_order_cast_toggle", "unit_order_stop", "unit_order_taunt", "unit_order_buyback", "unit_order_glyph", "unit_order_eject_item_from_stash", "unit_order_cast_rune", "unit_order_move_to_direction", "unit_order_patrol", "unit_order_vector_target_position", "unit_order_radar","unit_order_stop", "unit_order_set_item_combine_lock", "unit_order_continue"),function_NA) 

# High NA columns are all columns chosen in above function other than xp_other

High_NA_Columns <- c("xp_roshan","gold_killing_roshan","gold_buyback","unit_order_cast_toggle","unit_order_cast_toggle_auto","unit_order_drop_item","unit_order_give_item","unit_order_disassemble_item", "unit_order_cast_toggle", "unit_order_stop", "unit_order_taunt", "unit_order_buyback", "unit_order_glyph", "unit_order_eject_item_from_stash", "unit_order_cast_rune", "unit_order_move_to_direction", "unit_order_patrol", "unit_order_vector_target_position", "unit_order_radar","unit_order_stop", "unit_order_set_item_combine_lock", "unit_order_continue")

# Nullify all high NA columns so that the dataset is easier to work with

players_csv<-subset(players_csv,,-c(xp_roshan,gold_killing_roshan,gold_buyback,unit_order_cast_toggle,unit_order_cast_toggle_auto,unit_order_drop_item,unit_order_give_item,unit_order_disassemble_item, unit_order_cast_toggle, unit_order_stop, unit_order_taunt, unit_order_buyback, unit_order_glyph, unit_order_eject_item_from_stash, unit_order_cast_rune, unit_order_move_to_direction, unit_order_patrol, unit_order_vector_target_position, unit_order_radar,unit_order_stop, unit_order_set_item_combine_lock, unit_order_continue))

# Nullify Key column in objectives_csv due to high percentage of NA`s

objectives_csv <- subset(objectives_csv,,-key)

# Convert all negative account id`s in player_ratings_csv to 0

negative_account_id <- which(player_ratings_csv$account_id < 0)

player_ratings_csv$account_id[negative_account_id] <- 0
