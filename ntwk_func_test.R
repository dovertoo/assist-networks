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

#write function to select assist data for a specific game then you can use ntwk functions

test <- assist_data %>% 
     filter(game_id == 10479) %>% 
     #filter(home_team == "Maryland") %>% 
     filter(team_id == home_id)

md_hpu_data <- get_network_data(assist_data, "Maryland", "High Point", "2022-02-05", "home")

md_hpu_ntwk_nodes <- get_network_nodes(md_hpu_data)

md_hpu_ntwk_links <- get_network_links(md_hpu_data)


md_hpu_ntwk <- graph_from_data_frame(d=md_hpu_ntwk_links, vertices=md_hpu_ntwk_nodes, directed=T) 


plot_md_hpu_net <- ggraph(md_hpu_ntwk, layout = "kk") +
     geom_edge_arc(arrow = arrow(length = unit(5, 'mm'), type = "closed"), 
                   start_cap = circle(2, 'mm'),
                   end_cap = circle(4, 'mm'),
                   strength = 0.15,
                   aes(label = assister_shooter_freq_label,
                       edge_width = assister_shooter_freq),
                   show.legend = FALSE) +
     geom_node_point(aes(size = num_assists + 50),
                     shape = 21, fill = "black",
                     show.legend = FALSE) +   # add nodes to the plot
     geom_node_text(aes(label = player_name), size=5, color="gray50",
                    repel=T, nudge_x = -0.175, nudge_y = 0.14) +
     labs(caption = "Data: @laxreference | Plot: @danovertoom",
          title = "Maryland Assist Network",
          subtitle = "High Point vs Maryland - February 5th, 2022") +
     scale_edge_width_continuous(range = c(1.5,3)) +
     scale_size_continuous(range = c(4,12)) +
     theme_void() +
     theme(plot.title = element_text(size=22),
           plot.subtitle = element_text(size=14))

plot_md_hpu_net

md_hpu_ntwk_plot <- base_ntwk_plot(md_hpu_net, "gray25", "black") +
     ntwk_node_label("gray50", 0.175, 0.14) +
     labs(caption = "Data: @laxreference | Plot: @danovertoom",
          title = "Maryland Assist Network",
          subtitle = "High Point vs Maryland - February 5th, 2022") +
     theme(plot.title = element_text(size=22),
           plot.subtitle = element_text(size=14))

md_hpu_ntwk_plot