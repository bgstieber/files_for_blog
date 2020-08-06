library(tidyverse)
library(NbClust)
library(dbscan)
theme_set(theme_bw())

playlist <- readRDS("BradsJazzTracks.RDS")

playlist_features <- readRDS("BradsJazzTracksAudioFeatures.RDS")

playlist_features %>%
  select_if(is.numeric) %>%
  gather(variable, value) %>%
  ggplot(aes(value, fill = variable))+
  geom_histogram()+
  facet_wrap(~variable,scales = 'free')+
  theme(legend.position = "none")


vars_to_keep <- c("acousticness", "danceability", 
                  "duration_ms", "energy", 
                  "instrumentalness", 
                  "liveness", "loudness",
                  "speechiness", "tempo", 
                  "valence")

vars_to_log1p <- c("duration_ms",  
                 "liveness",
                 "speechiness",
                 "valence")

playlist_features.numeric <- playlist_features %>%
  select(all_of(vars_to_keep))

playlist_features.scale <- playlist_features.numeric %>%
  mutate_at(vars_to_log1p, log1p) %>%
  mutate_all(scale)


playlist_features.scale %>%
  gather(variable, value) %>%
  ggplot(aes(value, fill = variable))+
  geom_histogram()+
  facet_wrap(~variable,scales = 'free')


nbc <- NbClust(playlist_features.scale,
               method = "kmeans",
               max.nc = 10)

nbc_pc5 <- NbClust(predict(prcomp(playlist_features.scale))[,1:5],
                   method = "kmeans",
                   max.nc = 10)

km_2 <- kmeans(playlist_features.scale,centers = 2,iter.max = 100,nstart = 25)
km_6 <- kmeans(playlist_features.scale,centers = 6,iter.max = 100,nstart = 25)


km_data <- lapply(2:6,
                  FUN = function(k){
                    km <-  kmeans(playlist_features.scale,
                                  centers = k,
                                  iter.max = 100,
                                  nstart = 25)
                    
                    cluster_count <- as.data.frame(table(km$cluster)) %>%
                      mutate(clusters = as.numeric(Var1)) %>%
                      select(-Var1)
                    
                    km$centers %>%
                      as_tibble() %>%
                      mutate(cluster = k,
                             clusters = 1:k) %>%
                      gather(variable, value, -cluster, -clusters) %>%
                      inner_join(cluster_count)}) %>%
  bind_rows()


km_data %>%
  mutate(clusters = paste0(clusters, " (n=", Freq, ")")) %>%
  ggplot(aes(clusters, variable, fill = value))+
  geom_tile()+
  scale_fill_gradient2(limits = c(-2.5, 2.5),
                       oob = scales::squish)+
  facet_wrap(~cluster, scales = 'free_x')


playlist_features %>%
  arrange(instrumentalness) %>%
  select(id, speechiness, instrumentalness) %>%
  inner_join(playlist %>% select(id = track.id, track.name, track.artists)) %>%
  top_n(20, -instrumentalness)

