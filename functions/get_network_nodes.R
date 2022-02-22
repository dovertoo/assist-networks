#get_network_nodes(asst_data)


library(janitor)
library(tidyverse)

get_network_nodes <- function(asst_data){
     
     cnt_assists <- asst_data %>% 
          select(assister) %>% 
          count(assister) %>% 
          rename(num_assists = n)
     
     ntwk_nodes <- asst_data %>% 
          select(assister, shooter) %>% #need to have all shooters/assisters listed as vertices
          pivot_longer(everything(), values_to = "assister_shooter") %>% 
          select(-name) %>% 
          count(assister_shooter) %>% 
          rename(num_interactions = n) %>% 
          left_join(., cnt_assists, by = c("assister_shooter" = "assister")) %>% 
          mutate(num_assists = replace_na(num_assists, 0)) %>% 
          distinct() %>% 
          mutate(player_name = assister_shooter)
     
     #return(cnt_assists)
     return(ntwk_nodes)
     
}


