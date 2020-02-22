library(rvest)
library(tidyverse)
library(scales)
library(extrafont)
loadfonts()

theme_bgs <- function(){
  theme_bw() +
    theme(text = element_text(family = 'Segoe UI'),
          plot.title = element_text(face = 'plain', size = 14),
          plot.subtitle = element_text(family = 'Segoe UI Semibold'),
          panel.border = element_rect(colour = 'grey85'),
          panel.grid.minor = element_line(colour = "grey98", size = 0.25),
          axis.title = element_text(family = 'Segoe UI Semibold', size = 12),
          axis.text = element_text(size = 12),
          axis.ticks = element_blank(),
          legend.justification = 'top',
          legend.title = element_text(family = 'Segoe UI Semibold'),
          strip.background = element_rect(fill = 'grey92'),
          strip.text = element_text(family = 'Segoe UI Semibold'))
}

theme_set(theme_bgs())

scrape_pga <- function(url){
  url %>%
    read_html() %>%
    html_table(fill = TRUE) %>%
    .[[2]]
}


scrape_pga_tryCatch <- function(url){
  
  tryCatch(scrape_pga(url),
           error = function(e) "error")
  
}

clean_up_number <- function(x)  as.numeric(gsub(",", "", x))

putting_stats_df.base <- read_csv("putting_url_stats.csv")

putting_stats_list <- vector("list", nrow(putting_stats_df.base))
tpb <- txtProgressBar(min = 1, max = nrow(putting_stats_df.base))

t1 <- Sys.time()

for (i in 1:nrow(putting_stats_df.base)){
  
  putting_stats_list[[i]] <- scrape_pga_tryCatch(putting_stats_df.base$url[i])
  
  setTxtProgressBar(tpb, value = i)
}


t2 <- Sys.time()

putting_stats_df.base$data <- putting_stats_list

putting_stats <- putting_stats_df.base %>%
  rowwise() %>%
  mutate(ncol_data = ncol(data)) %>%
  ungroup() %>%
  filter(ncol_data == 7)

putting_names <- c("name", "url", "year", 
                   "ncol_data", "RANK THIS WEEK", "RANK LAST WEEK", 
                   "PLAYER NAME", "ROUNDS", "% MADE", 
                   "ATTEMPTS", "PUTTS MADE")

putting_stats2.list <- lapply(split(putting_stats, putting_stats$name),
                         FUN = function(d) d %>%
                           unnest(data) %>%
                           mutate_at(c("ATTEMPTS", "PUTTS MADE"),
                                     clean_up_number) %>%
                           set_names(putting_names))

putting_stats2 <- do.call("rbind", putting_stats2.list) %>%
  mutate(pct_made = `% MADE` / 100) %>%
  select(-`% MADE`) %>%
  rename(rank_this_week = `RANK THIS WEEK`,
         rank_last_week = `RANK LAST WEEK`,
         player = `PLAYER NAME`,
         rounds = ROUNDS,
         attempts = ATTEMPTS,
         putts_made = `PUTTS MADE`)

write_csv(putting_stats2, "putting_stats_09_19.csv")




