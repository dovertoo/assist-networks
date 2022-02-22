#ntwk_node_label <- function(txt_clr, x_coord, y_coord)


library(tidyverse)
library(igraph)
library(ggraph)

ntwk_node_label <- function(txt_clr, x_coord, y_coord){
     
     node_txt <- geom_node_text(aes(label = player_name), size=5, color= txt_clr,
                    repel=T, nudge_x = x_coord, nudge_y = y_coord)
     
     return(node_txt)
     
}