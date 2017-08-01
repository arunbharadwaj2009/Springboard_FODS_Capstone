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
match_csv1 <- left_join(match_csv,cluster_regions_csv,by = "cluster")

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

# Write function to compute percentage of NA`s in different columns of any dataset
function_total_NA <- function(x,y){x %>% summarise(total = sum(is.na(y)))}

#function_total_NA <- function(x,y){x %>% summarise(total = sum(is.na(y))/nrow(y))}
#mapply(players_csv,c(players_csv$gold_destroying_structure,players_csv$gold_killing_creeps),function_total_NA)

function_total_NA(players_csv,players_csv$gold_killing_roshan)  


# Check observations in players_csv where gold_abandon is NA
goldabandon_NA <- (is.na(players_csv$gold_abandon)) # 479366 observations in gold_abandon are 0

# Remove gold_abandon column in players_csv since a large percentage of its values are NA
players_csv$gold_abandon <- NULL 

# Perform something similar for gold_killing_couriers column in players_csv
goldkillingcouriers_NA <- (is.na(players_csv$gold_killing_couriers))
players_csv$gold_killing_couriers <- NULL

unit_order_none_NA <- (is.na(players_csv$unit_order_none))
players_csv$unit_order_none <- NULL
