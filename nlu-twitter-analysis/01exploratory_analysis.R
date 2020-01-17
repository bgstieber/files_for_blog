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

followed_by_four_or_more <- nlu_following %>%
  count(user_id) %>%
  filter(n >= 4) 

followed_by_four_or_more_info <- map_df(followed_by_four_or_more$user_id,
                                        lookup_users)



# 28 followed by all five
followed_by_four_or_more_info$name %>% sort

# get 250 most recent tweets for (one corner of) golf twitter
golf_twitter_timelines <- c(followed_by_four_or_more$user_id,
                            nlu_info$user_id) %>%
  unique() %>%
  map_df(get_timeline, n = 250, include_rts = FALSE)


write_as_csv(golf_twitter_timelines,
             file_name = "golf_twitter_timelines.csv")
