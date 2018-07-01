library(spotifyr)
library(tidyverse)
library(scales)
theme_set(theme_bw())
access_token <- get_spotify_access_token()


dawes_uri <- "0CDUUM6KNRvgBFYIbWxJwV"

dawes_albums <- get_albums(dawes_uri)

dawes_tracks <- get_album_tracks(dawes_albums)

dawes_tracks_features <- get_track_audio_features(dawes_tracks) %>%
  select(-key, -mode, -time_signature, -key_mode)

dawes_track_popularity <- get_track_popularity(dawes_tracks)


dawes_tracks2 <- dawes_tracks %>%
  inner_join(dawes_tracks_features) %>%
  filter(album_name %in% c("North Hills", "Nothing Is Wrong", 
                           "Stories Dont End", "All Your Favorite Bands", 
                           "Were All Gonna Die", "Passwords"))

dawes_numeric <- dawes_tracks2 %>%
  select(c("danceability", "energy",
           "loudness", "speechiness", "acousticness", 
           "liveness", "valence", "tempo", "duration_ms"))



dawes_numeric %>%
  gather(variable, value) %>%
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(~variable, scales = 'free')


## right skewed, use log
## acousticness, duration_ms, liveness, speechiness,
## valence

dawes_numeric_log <- dawes_numeric %>%
  mutate_at(c('acousticness',
              'duration_ms',
              'liveness',
              'speechiness',
              'valence'),
            funs(log(.+0.1))) %>%
  mutate(loudness = log(-loudness))


dawes_numeric_log %>%
  gather(variable, value) %>%
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(~variable, scales = 'free')


dawes_numeric_log_scaled <- as.data.frame(scale(dawes_numeric_log))

# dawes_numeric_log_scaled %>%
#   gather(variable, value)%>%
#   ggplot(aes(reorder(variable, value, median), value))+
#   geom_jitter(aes(colour = variable),
#               alpha = 0.8,
#               position = position_jitter(width = 0.2))+
#   theme(legend.position = 'none')+
#   scale_y_continuous(limits = c(-4, 4))

albums_and_numeric <- dawes_tracks2 %>%
  select(album_name) %>%
  cbind(dawes_numeric_log_scaled) %>%
  mutate(album_name = factor(album_name, 
                             c("North Hills", "Nothing Is Wrong", 
                               "Stories Dont End", "All Your Favorite Bands", 
                               "Were All Gonna Die", "Passwords")))


albums_and_numeric_long <- albums_and_numeric %>%
  gather(variable, value, -album_name) 

ggplot(albums_and_numeric_long,
       aes(abbreviate(variable, 7), value))+
  geom_jitter(data = select(albums_and_numeric_long, -album_name),
              colour = 'grey', alpha = 0.2)+
  geom_jitter(aes(colour = album_name),
              alpha = 0.8,
              position = position_jitter(width = 0.2))+
  theme(legend.position = 'none')+
  scale_y_continuous(limits = c(-4, 4))+
  facet_wrap(~album_name)

albums_and_numeric
  group_by(album_name) %>%
  summarise_all(mean) %>%
  gather(variable, value, -album_name) %>%
  ggplot(aes(album_name, variable, fill = value))+
  geom_tile(colour = 'black')+
  scale_fill_gradient2(low = muted('blue'), high = muted('red'))

# use NbClust package to suggest # of clusters
library(NbClust)
kmeans_dawes <- NbClust(data = dawes_numeric_log_scaled, method = 'kmeans')

# try 3 clusters
km_3 <- kmeans(dawes_numeric_log_scaled,
               centers = 3,
               iter.max = 100,
               nstart = 20)



