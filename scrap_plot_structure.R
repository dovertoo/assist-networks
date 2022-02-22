md_hpu_net <- ggraph(net, layout = "kk") +
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
          subtitle = "High Point vs Maryland - February 5th, 2022") +
     scale_edge_width_continuous(range = c(1.5,3)) +
     scale_size_continuous(range = c(4,12)) +
     theme_void() +
     theme(plot.title = element_text(size=22),
           plot.subtitle = element_text(size=14))