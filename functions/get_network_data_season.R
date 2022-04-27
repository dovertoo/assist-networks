#get_network_data_season <- function(asst_data, szn_tm, szn_tm_lg)


library(tidyverse)

get_network_data_season <- function(asst_data, szn_tm, szn_tm_lg){
     
     season_ntwk_data_home <- asst_data %>% 
          filter(home_team == szn_tm) %>% 
          filter(league == szn_tm_lg) %>% 
          filter(team_id == home_id)
     
     season_ntwk_data_away <- asst_data %>% 
          filter(away_team == szn_tm) %>% 
          filter(league == szn_tm_lg) %>% 
          filter(team_id == away_id) 
     
     season_ntwk_data <- bind_rows(season_ntwk_data_home, season_ntwk_data_away)
          
     return(season_ntwk_data)
     
}
