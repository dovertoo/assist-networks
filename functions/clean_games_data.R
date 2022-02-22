#clean_games_data(gms_data, tms_data)


library(janitor)
library(tidyverse)

clean_games_data <- function(gms_data, tms_data){
     
     teams_data <- tms_data %>% 
          clean_names() %>% 
          rename(team_id = id) %>% 
          select(-league)
     
     games_data <- gms_data %>% 
          clean_names() %>% 
          rename(game_id = id) %>% 
          left_join(., teams_data, by = c("home_id" = "team_id")) %>% #add home team name, etc.
          rename(home_team_short = short_code, home_team = display_name) %>% 
          left_join(., teams_data, by = c("away_id" = "team_id")) %>% #add away team name, etc.
          rename(away_team_short = short_code, away_team = display_name)
     
     return(games_data)
     #return(teams_data)
     
}


