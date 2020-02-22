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

keep_stats <- c("Putting from 4'", 
                "Putting from 5'", 
                "Putting from 6'", 
                "Putting from 7'", 
                "Putting from 8'", 
                "Putting from 9'", 
                "Putting from 10'")

putting_from <- putting_stats2 %>%
  filter(name %in% keep_stats)

putting_from.summary <- putting_from %>%
  group_by(name) %>%
  summarise(best = max(pct_made),
            worst = min(pct_made),
            median = median(pct_made),
            q75 = quantile(pct_made, .75),
            q25 = quantile(pct_made, .25))