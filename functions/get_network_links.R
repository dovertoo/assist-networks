#get_network_links(ntwk_data)


library(janitor)
library(tidyverse)

get_network_links <- function(ntwk_data){
     
     assister_shooter_combo <- ntwk_data %>% 
          select(assister, shooter) %>% 
          count(assister, shooter) %>% 
          rename(assister_shooter_freq = n) %>% 
          filter(assister_shooter_freq > 1)
     
     ntwk_links <- ntwk_data %>% 
          select(assister, shooter) %>% #define all connections between assisters and shooters
          left_join(., assister_shooter_combo, by = c("assister", "shooter")) %>% 
          mutate(assister_shooter_freq_label = replace_na(assister_shooter_freq, "")) %>% 
          mutate(assister_shooter_freq = replace_na(assister_shooter_freq, 0))
     
     
     #return(assister_shooter_combo)
     return(ntwk_links)
     
}