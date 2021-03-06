library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

## From here to Line 68 - Data Wrangling

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

# Download detailed heroes csv file from Louis Github, a former Springboard student who has developed a file with names of heroes, their types and other attributes. From Detailed_Heroes, subset 2 columns: hero_id and Class or type of hero. Heroes_Type table is small with only the 2 required columns

Heroes_Type <- Detailed_Heroes[ ,c(1,13)]

# Use left join to include the type of hero in players_csv table 

players_csv <- left_join(players_csv,Heroes_Type,by="hero_id")

# Reduce players_csv and players_ratings_csv file to only those rows that have non-zero account id`s

players_csv1 <- subset(players_csv,account_id != 0)
players_ratings_csv1 <- subset(player_ratings_csv,account_id != 0)

# In players_csv1, 0 to 4 stands for Radiant team and 128-132 stands for Dire team

players_csv1$player_slot <- sub(pattern="^0",replacement = "Radiant",players_csv1$player_slot)
players_csv1$player_slot <- sub(pattern="^1$",replacement = "Radiant",players_csv1$player_slot)
players_csv1$player_slot <- sub(pattern="^2$",replacement = "Radiant",players_csv1$player_slot)
players_csv1$player_slot <- sub(pattern="^3$",replacement = "Radiant",players_csv1$player_slot)
players_csv1$player_slot <- sub(pattern="^4$",replacement = "Radiant",players_csv1$player_slot)

players_csv1$player_slot <- sub(pattern="128",replacement = "Dire",players_csv1$player_slot)
players_csv1$player_slot <- sub(pattern="129",replacement = "Dire",players_csv1$player_slot)
players_csv1$player_slot <- sub(pattern="130",replacement = "Dire",players_csv1$player_slot)
players_csv1$player_slot <- sub(pattern="131",replacement = "Dire",players_csv1$player_slot)
players_csv1$player_slot <- sub(pattern="132",replacement = "Dire",players_csv1$player_slot)

## In players_csv3 item_0, item_1, item_2, item_3, item_4 and item_5 to be converted to actual item names

# This piece of code was reused to convert item_0, item_1, item_2, item_3, item_4 and item_5 columns from item_id to item_name

colnames(players_csv3)[colnames(players_csv3)=="item_5"] <- "item_id"
players_csv3 <- left_join(players_csv3,item_ids_csv,by="item_id") 
colnames(players_csv3)[colnames(players_csv3)=="item_name"] <- "item_5"
players_csv3$item_id <- NULL

### Exploratory Data Analysis and Data Visualization using ggplot2

# Calculate total stats by hero type

Stats_by_hero_type <- na.omit(players_csv) %>% group_by(Class) %>% summarise(Total_Damage = sum(hero_damage),Total_Healing = sum(hero_healing),Total_Kills = sum(kills),Total_Deaths = sum(deaths),Total_Assists = sum(assists),Total_Tower_Damage = sum(tower_damage),Total_Gold_Spent = sum(gold_spent,na.rm=TRUE),Last_Hits = sum(last_hits),Total_Stuns=sum(stuns),Mean_XP_Per_Min=mean(xp_per_min),Mean_Gold_Per_Min=mean(gold_per_min),Runes_Picked=sum(unit_order_pickup_rune,na.rm=TRUE))

# Scale all observations so that sum of columns equals 1. This helps normalize values

Scaled_Stats_by_hero_type <- Stats_by_hero_type

Scaled_Stats_by_hero_type <- sapply(Scaled_Stats_by_hero_type[,-1],function(x) round(x/sum(x),2))

Class <- c("AGI","INT","STR")

row.names(Scaled_Stats_by_hero_type) <- Class

# Visualize scaled stats using heatmap so that reader gets complete picture of hero stats from single graph

Scaled_Stats_by_hero_type_m <- melt(Scaled_Stats_by_hero_type)

ggplot(Scaled_Stats_by_hero_type_m, aes(x=Var1,y=Var2,fill=value)) + geom_tile() + labs(x="Type of Hero",y="Stats") + scale_fill_gradient(low="white",high="red")

# Better to only look at players who have played a minimum of 5 games. So, players_ratings_csv2 only includes those players 

players_ratings_csv2 <- players_ratings_csv1 %>% filter(total_matches>4)

# Insert new column to find percentage of wins

players_ratings_csv2 <- players_ratings_csv2 %>% mutate(percentage_wins=total_wins/total_matches)

# Histogram line plot of percentage_wins statistic by player account_id. Most players win half of the games they play.

ggplot(players_ratings_csv2,aes(percentage_wins)) +geom_histogram(aes(y=..density..)) + geom_density(aes(y=..density..))

# Scatter plot of trueskill_mu and percentage_wins in players_ratings_csv2. trueskill_sigma is assigned to color attribute. Both trueskill attributes are calculated by Dota fans and not by Valve corporation. While trueskill_mu is indicator of player`s ability, with higher value implying better player, trueskill_sigma is the uncertainty in the trueskill_mu measure. As expected, skill and win percentage are correlated. 

ggplot(players_ratings_csv2,aes(x=trueskill_mu,y=percentage_wins,col=trueskill_sigma)) +geom_point() + geom_jitter(shape=1)

# Scatter plot of radiant_win and game duration shows that radiant or dire winning is unaffected by game duration (radiant and dire are the names of 2 dota teams)

ggplot(match_csv,aes(x=radiant_win,y=duration)) +geom_point() + geom_jitter(shape=1)


