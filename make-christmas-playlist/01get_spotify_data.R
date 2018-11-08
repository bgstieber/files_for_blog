library(spotifyr)
library(tidyverse)
library(scales)
theme_set(theme_bw())
# connect
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')
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

clean_spotify <- function(data, mu = 0, sigma = 0, calc_scales = TRUE){
  
  data1 <- data %>%
    select(c("danceability", "energy", "loudness", "speechiness", "acousticness", 
             "instrumentalness", "liveness", "valence", "tempo", "duration_ms")) %>%
    mutate_at(c('duration_ms', 'instrumentalness',
                'liveness', 'speechiness', 'tempo'),
              log1p)
  
  
  apply(data1, 1, FUN = function(x) (x - mu) / sigma) %>%
    t() %>%
    as_data_frame()
  
}
