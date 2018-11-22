library(spotifyr)
library(tidyverse)
library(scales)
library(caret)
library(ggrepel)
library(RANN)
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
# PC2 heavily weights on valence (positivity), danceability, and acousticness
# Nat King Cole has multiple
full_pca %>%
  slice(which.max(PC2)) %>%
  inner_join(spotify_holiday) %>%
  select(track_name, artist_name)

full_pca %>%
  top_n(5, PC2) %>%
  inner_join(spotify_holiday) %>%
  select(track_name, artist_name)


## find nearest neighbor for each song in my playlist
dist_for_playists <- apply(holiday_playlist_features2_scaled[,-1],
      1,
      FUN = function(x){
        as.matrix(dist(
          rbind(x, spotify_holiday_features2_scaled[,-1])))[-1,1]
    })

table(apply(dist_for_playists, 2, which.min))

# another way
nn_data_k1 <- nn2(spotify_holiday_features2_scaled[,-1],
               holiday_playlist_features2_scaled[,-1],
               k = 1)

nn_data_k5 <- nn2(spotify_holiday_features2_scaled[,-1],
                  holiday_playlist_features2_scaled[,-1],
                  k = 5)

nn_data_k10 <- nn2(spotify_holiday_features2_scaled[,-1],
                  holiday_playlist_features2_scaled[,-1],
                  k = 10)
# 21 songs that are 5th or nearer neighbors of
# at least two songs
sum((t_idx <- table(as.numeric(nn_data_k5$nn.idx))) > 1)

indices_k5 <- as.numeric(names(t_idx[t_idx > 1]))

close_neighbors <- spotify_holiday_features2_scaled[indices_k5,] %>%
  inner_join(spotify_holiday) %>%
  select(artist_name, track_name, track_uri)

full_pca %>%
  left_join(close_neighbors %>% mutate(neighbor = T)) %>%
  mutate(neighbor_alpha = ifelse(type == 'training',
                                 'y',
                                 ifelse(is.na(neighbor),
                                        'n', 'y'))) %>%
  ggplot(aes(PC1, PC2, colour = type))+
  geom_point(aes(alpha = neighbor_alpha))+
  scale_colour_manual(values = c('training' = 'red',
                                 'testing' = 'green'))+
  scale_alpha_manual(values = c('y' = 1, 'n' = 0.15))

# 60 songs that are 510h or nearer neighbors of
# at least two songs
sum(table(as.numeric(nn_data_k10$nn.idx)) > 1)

# fit a logistic regression, grab X highest predicted
# probabilities for spotify playlist
full_pca$playlist <- as.numeric(full_pca$type == 'training')

xmas_model <- glm(playlist ~ PC1 + PC2 + PC3 + PC4,
                  data = full_pca,
                  family = 'binomial')

full_pca$pred_prob <- predict(xmas_model, type = 'response')

# top 30 songs

full_pca %>%
  filter(type == 'testing') %>%
  top_n(30, pred_prob)


