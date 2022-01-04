library(janitor)
library(tidyverse)
library(igraph)


player_data <- read.csv("data/md_player_data_2021.csv") %>% 
     clean_names() %>% 
     rename(player_id = id) %>% 
     select(player, player_id)
games_data <- read.csv("data/md_games_data_2021.csv") %>% 
     clean_names() %>% 
     rename(game_id = id)
raw_assist_data <- read.csv("data/md_assist_data_2021.csv") %>% 
     clean_names()
#clean up assist data
assist_data <- raw_assist_data %>% 
     left_join(., player_data, by = c("shooter_id" = "player_id")) %>% #add shooter name
     rename(shooter = player) %>% 
     left_join(., player_data, by = c("assister_id" = "player_id")) %>% #add assister name
     rename(assister = player) %>% 
     filter(game_id == 9206) #md vs hopkins on 3/6/2021

nodes <- assist_data %>% 
     select(assister, shooter) %>% #need to have all shooters/assisters listed as vertices
     pivot_longer(everything(), values_to = "assister_shooter") %>% 
     select(-name) %>% 
     distinct()

links <- assist_data %>% 
     select(assister, shooter) #define all connections between assisters and shooters

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

#set vertex size based on number of interactions
deg <- degree(net, mode="all")
V(net)$size <- deg*4

# Set edge width based on weight:
E(net)$width <- E(net)$weight


l <- layout_with_kk(net)

#need to play with layouts??
plot(net, vertex.label.degree = pi/2, vertex.label.dist = 3, layout = l) #layout.fruchterman.reingold



