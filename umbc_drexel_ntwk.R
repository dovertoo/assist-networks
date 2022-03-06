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

#get network data for a specific game
umbc_drexel_data <- get_network_data(assist_data, "UMBC", "Drexel", "2022-02-19", "home")
#gather network nodes
umbc_drexel_ntwk_nodes <- get_network_nodes(umbc_drexel_data)
#gather network links
umbc_drexel_ntwk_links <- get_network_links(umbc_drexel_data)

#create network object for specific game selected above
umbc_drexel_ntwk <- graph_from_data_frame(d=umbc_drexel_ntwk_links, vertices=umbc_drexel_ntwk_nodes, directed=T) 

#plot network
umbc_drexel_ntwk_plot <- base_ntwk_plot(umbc_drexel_ntwk, "gray25", "black") +
     ntwk_node_label("gray50", 0.175, 0.14) +
     labs(caption = "Data: @laxreference | Plot: @danovertoom",
          title = "UMBC Assist Network",
          subtitle = "Drexel vs UMBC - February 5th, 2022") +
     theme(plot.title = element_text(size=22),
           plot.subtitle = element_text(size=14))

umbc_drexel_ntwk_plot