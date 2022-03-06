#clean_player_data(plyr_data)


library(janitor)
library(tidyverse)

clean_player_data <- function(plyr_data){
     
     player_data <- plyr_data %>% 
          clean_names() %>% 
          rename(player_id = id) %>% 
          separate(player, into = c("player_first", "player_last"), extra = "merge", sep = " ") %>% 
          #separate(player, into = c("player_first", "player_last1", "player_last2", "player_last3"), sep = " ") %>% 
          #mutate(player_last2 = replace_na(player_last2, ""), player_last3 = replace_na(player_last3, "")) %>% 
          #unite(col = "player_last", c("player_last1", "player_last2", "player_last3"), sep = " ") %>% 
          mutate(player = paste0(str_sub(player_first, start = 1, end = 1), ". ", player_last)) %>% 
          select(player, player_id)
          #figure out what to do with team???
     return(player_data)

}


