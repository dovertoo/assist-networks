#base_ntwk_plot_game <- function(ntwk, edge_clr, node_clr)


library(tidyverse)
library(igraph)
library(ggraph)

base_ntwk_plot_game <- function(ntwk, edge_clr, node_clr){
     
     base_ntwk_game <- ggraph(ntwk, layout = "kk") +
          geom_edge_arc(arrow = arrow(length = unit(5, 'mm'), type = "closed"), 
                        start_cap = circle(2, 'mm'),
                        end_cap = circle(4, 'mm'),
                        strength = 0.15,
                        color = edge_clr,
                        aes(label = assister_shooter_freq_label,
                            edge_width = assister_shooter_freq),
                            #edge_color = edge_clr),
                        show.legend = FALSE) +
          geom_node_point(aes(size = num_assists + 50),
                          shape = 21, fill = node_clr,
                          show.legend = FALSE) +  # add nodes to the plot
          scale_edge_width_continuous(range = c(1.5,3)) +
          scale_size_continuous(range = c(4,12)) +
          theme_void()
     return(base_ntwk_game)
     
}