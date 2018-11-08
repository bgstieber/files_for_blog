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
