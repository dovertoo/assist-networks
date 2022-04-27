#clean_assist_data(asst_data, plyr_data, gms_data)


library(janitor)
library(tidyverse)

clean_assist_data <- function(asst_data, plyr_data, gms_data){
     
     assist_data <- asst_data %>% 
          clean_names() %>% 
          left_join(., plyr_data, by = c("shooter_id" = "player_id")) %>% #add shooter name
          rename(shooter = player) %>% 
          left_join(., plyr_data, by = c("assister_id" = "player_id")) %>% #add assister name
          rename(assister = player) %>% 
          left_join(., gms_data, by = "game_id") %>% 
          distinct()
     
     return(assist_data)
     
}


