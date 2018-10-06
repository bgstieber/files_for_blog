library(rtweet)
library(tidyverse)

nlu_guys <- c('NoLayingUp', 
              'troncarternlu', 
              'bigrandynlu',
              'ngschuNLU',
              'djpie') 

nlu_following <- map_df(nlu_guys, get_friends)

nlu_info <- map_df(nlu_guys, lookup_users)

nlu_following %>%
  count(user_id, sort = TRUE) %>%
  count(n) %>%
  ggplot(aes(n, nn))+
  geom_col()

followed_by_all <- nlu_following %>%
  count(user_id) %>%
  filter(n == 5) 

followed_by_all_info <- map_df(followed_by_all$user_id,
                               lookup_users)



# 28 followed by all five
followed_by_all_info$name %>% sort