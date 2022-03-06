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

home_team <- "Maryland"
away_team <- "Florida"
game_date <- "2022-02-26"
assist_team <- "home"

assist_ntwk_tm <- ifelse(assist_team == "home", home_team, away_team)

#get network data for a specific game
ntwk_data <- get_network_data(assist_data, home_team, away_team, game_date, assist_team)
#gather network nodes
ntwk_nodes <- get_network_nodes(ntwk_data)
#gather network links
ntwk_links <- get_network_links(ntwk_data)

#create network object for specific game selected above
curr_ntwk <- graph_from_data_frame(d=ntwk_links, vertices=ntwk_nodes, directed=T) 

#plot network
curr_ntwk_plot <- base_ntwk_plot(curr_ntwk, "#FF0000", "black") +
     ntwk_node_label("gray50", 0.175, -0.14) +
     labs(caption = "Data: @laxreference | Plot: @danovertoom",
          title = paste(assist_ntwk_tm, "Assist Network"),
          subtitle = paste(away_team, "vs.", home_team, game_date)) +
     theme(plot.title = element_text(size=22),
           plot.subtitle = element_text(size=14),
           plot.caption = element_text(size=12))

curr_ntwk_plot

ggsave(filename = paste0("plots/", game_date, "_", home_team, "_", away_team, ".png"), plot = curr_ntwk_plot, dpi = 800)

