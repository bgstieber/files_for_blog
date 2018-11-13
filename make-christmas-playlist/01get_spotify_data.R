library(spotifyr)
library(tidyverse)
library(scales)
library(caret)
library(ggrepel)
theme_set(theme_bw())
# connect
access_token <- get_spotify_access_token()

playlist_info <- 'spotify:user:12102534356:playlist:6IWeDaYa1NQLzPWWcSh2GL'
# get my playlists
my_playlists <- get_user_playlists('12102534356') 
# get playlist and playlist track info
## my curated list
holiday_playlist <- my_playlists  %>%
  filter(playlist_name == 'holiday_playlist') %>%
  get_playlist_tracks()

holiday_playlist_features <- holiday_playlist %>%
  get_track_audio_features()

## collection of spotify songs
spotify_holiday <- my_playlists %>%
  filter(playlist_name == 'spotify_holiday_playlists') %>%
  get_playlist_tracks() %>%
  # filter duplicates
  filter(!duplicated(.[c('track_name', 'artist_name')] %>%
           mutate_all(toupper)))

spotify_holiday_features <- spotify_holiday %>%
  get_track_audio_features()


# visualize numeric variables

holiday_playlist_features %>%
  select_if(is.numeric) %>%
  gather(variable, value) %>%
  ggplot(aes(value, fill = variable))+
  geom_histogram()+
  facet_wrap(~variable, scales = 'free')

spotify_holiday_features %>%
  select_if(is.numeric) %>%
  gather(variable, value) %>%
  ggplot(aes(value, fill = variable))+
  geom_histogram()+
  facet_wrap(~variable, scales = 'free')

# based on larger data set, should do the following
# exclude: time_signature
# log-scale: duration_ms, instrumentalness, liveness, speechiness, and tempo
# then, center and scale all variables

holiday_playlist_features2 <- holiday_playlist_features %>%
  select(c("track_uri", "danceability", "energy", "loudness", "speechiness", "acousticness", 
           "instrumentalness", "liveness", "valence", "tempo", "duration_ms")) %>%
  mutate_at(c('duration_ms', 'instrumentalness',
              'liveness', 'speechiness', 'tempo'),
            log1p)

spotify_holiday_features2 <- spotify_holiday_features %>%
  select(c("track_uri", "danceability", "energy", "loudness", "speechiness", "acousticness", 
           "instrumentalness", "liveness", "valence", "tempo", "duration_ms")) %>%
  mutate_at(c('duration_ms', 'instrumentalness',
              'liveness', 'speechiness', 'tempo'),
            log1p)

# center and scale, using my playlist as "training" set
pre_proc <- preProcess(holiday_playlist_features2,
                       method = c('center', 'scale'))

holiday_playlist_features2_scaled <- predict(pre_proc, 
                                             holiday_playlist_features2)

spotify_holiday_features2_scaled <- predict(pre_proc,
                                            spotify_holiday_features2)

# fit principal components (dimensionality reduction) on playlist
prco_holiday <- prcomp(holiday_playlist_features2_scaled[,-1])

pc_rotation <- prco_holiday$rotation %>%
  as_data_frame() %>%
  mutate(variable = row.names(prco_holiday$rotation))

training_pca <- predict(prco_holiday,
                        newdata = holiday_playlist_features2_scaled) %>%
  as_data_frame() %>%
  mutate(type = 'training') %>%
  bind_cols(holiday_playlist_features2_scaled)

testing_pca <- predict(prco_holiday, 
                       newdata = spotify_holiday_features2_scaled) %>%
  as_data_frame() %>%
  mutate(type = 'testing') %>%
  bind_cols(spotify_holiday_features2_scaled)

full_pca <- training_pca %>%
  bind_rows(testing_pca)

pc_rotation %>%
  gather(pc, value, starts_with('PC')) %>%
  filter(pc %in% paste0('PC', 1:4)) %>%
  ggplot(aes(pc, variable, fill = value))+
  geom_tile(colour = 'grey20')+
  scale_fill_gradient2()

full_pca %>%
  filter(type == 'training') %>%
  mutate(song = holiday_playlist$track_name) %>%
  ggplot(aes(PC1, PC2))+
  geom_point()+
  geom_text_repel(aes(label = song))

full_pca %>%
  ggplot(aes(PC1, PC2, colour = type))+
  geom_point()+
  scale_colour_manual(values = c('training' = 'red',
                                 'testing' = 'green'))

## investigate some outliers
