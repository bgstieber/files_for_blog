library(spotifyr)
library(tidyverse)

get_spotify_access_token()


dawes_uri <- "0CDUUM6KNRvgBFYIbWxJwV"

dawes_albums <- get_albums(dawes_uri)

dawes_tracks <- get_album_tracks(dawes_albums) %>%

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
           "loudness", "speechiness", "acousticness", "instrumentalness", 
           "liveness", "valence", "tempo", "duration_ms", "time_signature"))



dawes_numeric %>%
  gather(variable, value) %>%
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(~variable, scales = 'free')


dawes_numeric2 <- dawes_numeric %>%
  select(-instrumentalness, -time_signature)

## right skewed, use log
## acousticness, duration_ms, liveness, speechiness,
## valence

dawes_numeric2_log <- dawes_numeric2 %>%
  mutate_at(c('acousticness',
              'duration_ms',
              'liveness',
              'speechiness',
              'valence'),
            funs(log(.+0.1))) %>%
  mutate(loudness = log(-loudness))


dawes_numeric2_log %>%
  gather(variable, value) %>%
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(~variable, scales = 'free')


dawes_numeric2_log_scaled <- as.data.frame(scale(dawes_numeric2_log))

dawes_numeric2_log_scaled %>%
  gather(variable, value)%>%
  ggplot(aes(variable, value))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(colour = variable),
              alpha = 0.8,
              position = position_jitter(width = 0.2))+
  theme(legend.position = 'none')





