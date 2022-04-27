#get_network_data_game <- function(asst_data, hm_team, awy_team, gm_date, home_away)


library(tidyverse)

get_network_data_game <- function(asst_data, hm_team, awy_team, gm_date, home_away){
     
     game_ntwk_data <- asst_data %>% 
          filter(game_date == gm_date & home_team == hm_team & away_team == awy_team) %>%
          filter(case_when(
                    home_away == "home" ~ team_id == home_id,
                    home_away == "away" ~ team_id == away_id
               )
          )
          
     return(game_ntwk_data)
     
}
