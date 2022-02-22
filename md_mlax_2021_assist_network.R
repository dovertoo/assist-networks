library(janitor)
library(tidyverse)
library(igraph)
library(ggraph)


player_data <- read.csv("data/md_player_data_2021.csv") %>% 
     clean_names() %>% 
     rename(player_id = id) %>% 
     separate(player, into = c("player_first", "player_last")) %>% 
     mutate(player = paste0(str_sub(player_first, start = 1, end = 1), ". ", player_last)) %>% 
     select(player, player_id)
games_data <- read.csv("data/md_games_data_2021.csv") %>% 
     clean_names() %>% 
     rename(game_id = id)
raw_assist_data <- read.csv("data/md_assist_data_2021.csv") %>% 
     clean_names()

teams_data <- read.csv("data/data_teams.csv") %>% 
        clean_names() %>% 
        rename(team_id = id)

#clean up assist data
assist_data <- raw_assist_data %>% 
     left_join(., player_data, by = c("shooter_id" = "player_id")) %>% #add shooter name
     rename(shooter = player) %>% 
     left_join(., player_data, by = c("assister_id" = "player_id")) %>% #add assister name
     rename(assister = player) %>% 
     filter(game_id == 9206) #md vs hopkins on 3/6/2021

#total number of assists each player had 
assists <- assist_data %>% 
        select(assister) %>% 
        count(assister) %>% 
        rename(num_assists = n) 

nodes <- assist_data %>% 
     select(assister, shooter) %>% #need to have all shooters/assisters listed as vertices
     pivot_longer(everything(), values_to = "assister_shooter") %>% 
     select(-name) %>% 
     count(assister_shooter) %>% 
     rename(num_interactions = n) %>% 
     left_join(., assists, by = c("assister_shooter" = "assister")) %>% 
     mutate(num_assists = replace_na(num_assists, 0)) %>% 
     distinct() %>% 
     mutate(player_name = assister_shooter)

assister_shooter_combo <- assist_data %>% 
        select(assister, shooter) %>% 
        count(assister, shooter) %>% 
        rename(assister_shooter_freq = n) %>% 
        filter(assister_shooter_freq > 1)

links <- assist_data %>% 
     select(assister, shooter) %>% #define all connections between assisters and shooters
     left_join(., assister_shooter_combo, by = c("assister", "shooter")) %>% 
     mutate(assister_shooter_freq_label = replace_na(assister_shooter_freq, "")) %>% 
     mutate(assister_shooter_freq = replace_na(assister_shooter_freq, 0))
        
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

#set vertex size based on number of interactions
deg <- degree(net, mode="all")
V(net)$size <- deg*4

# Set edge width based on number of assists
E(net)$width <- E(net)$num_assists

#kk best layout so far - gives consistent layout when saved to a variable
#l <- layout_with_kk(net)

l <- layout_with_kk(net)

#need to play with layouts??
plot(net, vertex.label.degree = pi+0.85, 
     vertex.label.dist = 3.6, 
     layout = l,
     main = "Maryland Assist Network",
     sub = "Hopkins vs Maryland - March 6th, 2021") 


md_hop_net <- ggraph(net, layout = "kk") +
     geom_edge_arc(arrow = arrow(length = unit(5, 'mm'), type = "closed"), 
                      start_cap = circle(2, 'mm'),
                      end_cap = circle(4, 'mm'),
                      strength = 0.15,
                      aes(label = assister_shooter_freq_label,
                          edge_width = assister_shooter_freq,
                          edge_color = "gray25"),
                      show.legend = FALSE) +
     geom_node_point(aes(size = num_assists + 50),
                     shape = 21, fill = "black",
                     show.legend = FALSE) +   # add nodes to the plot
     geom_node_text(aes(label = player_name), size=5, color="gray50",
                    repel=T, nudge_x = 0.175, nudge_y = 0.14) +
     labs(caption = "Data: @laxreference | Plot: @danovertoom",
          title = "Maryland Assist Network",
          subtitle = "Hopkins vs Maryland - March 6th, 2021") +
     scale_edge_width_continuous(range = c(1.5,3)) +
     scale_size_continuous(range = c(4,12)) +
     theme_void() +
     theme(plot.title = element_text(size=22),
           plot.subtitle = element_text(size=14))

md_hop_net

ggsave(filename = "plots/md_hop_net_test.png", plot = md_hop_net, dpi = 800)
