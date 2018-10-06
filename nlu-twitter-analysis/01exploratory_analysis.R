library(rtweet)
library(tidyverse)

nlu_guys <- c('NoLayingUp', 'troncarternlu', 'bigrandynlu',
              'ngschuNLU','djpie') 

nlu_following <- map_df(nlu_guys, get_friends)

nlu_info <- map_df(nlu_guys, lookup_users)


