library(janitor)
library(tidyverse)
library(igraph)
library(ggraph)

#read in all functions in functions/ directory

func_list <- list.files("functions/")

map(paste0("functions/", func_list), source)

#read in data and clean with helper functions

players_data <- read.csv("data/data_players_2022.csv") %>% 
     clean_player_data()

teams_data <- read.csv("data/data_teams.csv")

games_data <- read.csv("data/data_games_2022.csv") %>% 
     clean_games_data(., teams_data)

assist_data <- read.csv("data/data_assists_2022.csv") %>% 
     clean_assist_data(., players_data, games_data)

##########################
##game specific networks
##########################
game_home_team <- "High Point"
game_away_team <- "North Carolina"
game_date <- "2022-03-20"
game_assist_team <- "away"

game_assist_ntwk_tm <- ifelse(game_assist_team == "home", game_home_team, game_away_team)

#get network data for a specific game
game_ntwk_data <- get_network_data_game(assist_data, game_home_team, game_away_team, game_date, game_assist_team)
#gather network nodes
game_ntwk_nodes <- get_network_nodes(game_ntwk_data)
#gather network links
game_ntwk_links <- get_network_links(game_ntwk_data)

#create network object for specific game selected above
game_curr_ntwk <- graph_from_data_frame(d=game_ntwk_links, vertices=game_ntwk_nodes, directed=T) 

#plot network
game_curr_ntwk_plot <- base_ntwk_plot_game(game_curr_ntwk, "#0099CC", "#CCCCCC") +
     ntwk_node_label("gray50", 0.2, -0.18) +
     labs(caption = "Data: @laxreference | Plot: @danovertoom",
          title = paste(game_assist_ntwk_tm, "Assist Network"),
          subtitle = paste(game_away_team, "vs.", game_home_team, game_date)) +
     theme(plot.title = element_text(size=22),
           plot.subtitle = element_text(size=14),
           plot.caption = element_text(size=12))

game_curr_ntwk_plot

ggsave(filename = paste0("plots/", game_date, "_", game_home_team, "_", game_away_team, ".png"), plot = game_curr_ntwk_plot, dpi = 800)


##########################
##season specific networks
##########################

season_year <- "2022"
season_team <- "Maryland"
season_league <- "NCAA D1 Men"
season_lg_shrt <- if_else(season_league == "NCAA D1 Men", "Men", "Women")

#get network data for a specific team's season
season_ntwk_data <- get_network_data_season(assist_data, season_team, season_league)
#gather network nodes
season_ntwk_nodes <- get_network_nodes(season_ntwk_data)
#gather network links
season_ntwk_links <- get_network_links(season_ntwk_data)

#create network object for specific team-season selected above
season_curr_ntwk <- graph_from_data_frame(d=season_ntwk_links, vertices=season_ntwk_nodes, directed=T) 

#plot network
season_curr_ntwk_plot <- base_ntwk_plot_season(season_curr_ntwk, "#FF0000", "black") +
     #ntwk_node_label("gray50", 0.175, -0.14) +
     labs(caption = "Data: @laxreference | Plot: @danovertoom",
          title = paste(season_team, "Assist Network"),
          subtitle = paste(season_year, "Season -", season_team, season_lg_shrt)) +
     theme(plot.title = element_text(size=22),
           plot.subtitle = element_text(size=14),
           plot.caption = element_text(size=12))

season_curr_ntwk_plot

ggsave(filename = paste0("plots/", season_year, season_team, season_lg_shrt, ".png"), plot = season_curr_ntwk_plot, dpi = 800)



