library(tidyverse)
library(scales)
library(ggridges)

theme_set(theme_bw())

putting_stats2 <- read_csv("putting_stats_09_19.csv")

keep_stats <- c("Putting from 4'", 
                "Putting from 5'", 
                "Putting from 6'", 
                "Putting from 7'", 
                "Putting from 8'", 
                "Putting from 9'", 
                "Putting from 10'")

putting_from <- putting_stats2 %>%
  filter(name %in% keep_stats) %>%
  mutate(name = factor(name, levels = keep_stats))

putting_from.summary <- putting_from %>%
  group_by(name) %>%
  summarise(best = max(pct_made),
            worst = min(pct_made),
            median = median(pct_made),
            q75 = quantile(pct_made, .75),
            q25 = quantile(pct_made, .25))

putting_from.summary_long <- putting_from.summary %>%
  gather(measure, value, -name)

# interesting dropoffs
p1 <- putting_from.summary_long %>%
  mutate(measure = factor(measure,
                          levels = c("best", "q75",
                                     "median", "q25",
                                     "worst"))) %>%
  ggplot(aes(name, value, colour = measure, group = measure))+
  geom_line(size = 1.2)+
  geom_point(size = 2)+
  scale_colour_viridis_d("Summary Measure",
                         labels = c("Best",
                                    "75th Percentile",
                                    "Median",
                                    "25th Percentile",
                                    "Worst"))+
  scale_x_discrete('"Putting From" stat',
                   labels = function(x) gsub("Putting from ",
                                             "",
                                             x))+
  scale_y_continuous("% of Putts Made", labels = percent)+
  ggtitle("Summary Measures of Putting Performance",
          subtitle = "Data based on statistics from 2009 to 2019")+
  theme(legend.justification = 'top')

# difference between 75th and 25th percentile (how hard to go from bottom 25 to top 25?)
p2 <- putting_from.summary %>%
  mutate(q75_minus_q25 = q75 - q25) %>%
  ggplot(aes(name, 100 * q75_minus_q25))+
  geom_col()+
  geom_label(aes(label = round(100 * q75_minus_q25, 1)),
             vjust = 'top')+
  xlab("Stat")+
  ylab("Putts Made per 100 Attempts Difference")+
  ggtitle("Difference in Putts Made per 100 Attempts Between the 75th and 25th Percentiles",
          subtitle = "Data based on statistics from 2009 to 2019")

# some distributional overlap (some players are better than others at longer lengths)
p3 <- putting_from %>%
  ggplot(aes(pct_made, fct_rev(name), fill = name))+
  geom_density_ridges2(show.legend = FALSE,
                       rel_min_height = 0.01)+
  scale_fill_viridis_d(option = "inferno")+
  scale_x_continuous("% of Putts Made",
                     labels = percent)+
  ylab("Stat")+
  ggtitle("Distribution of Putting Statistics",
          subtitle = "Data based on statistics from 2009 to 2019")


save_plots <- FALSE

if(save_plots){
  
  ggsave("putting_summary.png",
         p1,
         width = 8, height = 6,
         units = "in", dpi = 350)
  
  ggsave("diff_q75_q25.png",
         p2,
         width = 8, height = 6,
         units = "in", dpi = 350)
  
  ggsave("stat_dist.png",
         p3,
         width = 8, height = 6,
         units = "in", dpi = 350)
}


# putting performance doesn't show a trend over time (09-19)
putting_from %>%
  ggplot(aes(year, pct_made, group = year))+
  geom_boxplot()+
  facet_wrap(~name, scales = 'free_y')

# putting performance (except for 4') seems to fit a gaussian 
putting_from %>%
  ggplot(aes(pct_made, group = year))+
  geom_density(fill = 'dodgerblue2', alpha = 0.3)+
  facet_wrap(~name, scales = 'free')



putting_from %>%
  group_by(year, name) %>%
  mutate(pct_made_scaled = scale(pct_made)) %>%
  ungroup() %>%
  filter(pct_made_scaled >= 3) %>%
  select(player, year, name, pct_made, pct_made_scaled) %>%
  arrange(desc(pct_made_scaled))

putting_from %>%
  group_by(year, name) %>%
  mutate(pct_made_scaled = scale(pct_made)) %>%
  ungroup() %>%
  filter(pct_made_scaled <= -3) %>%
  select(player, year, name, pct_made, pct_made_scaled) %>%
  arrange(pct_made_scaled)



putting_from_by_year_Wide <- putting_from %>%
  select(player, year, name, pct_made) %>%
  mutate(name = gsub("'", "", gsub(" ", "_", tolower(name)))) %>%
  group_by(player, year) %>%
  spread(name, pct_made)

putting_from_by_year_wide.attempts <- putting_from %>%
  select(player, year, name, attempts) %>%
  mutate(name = gsub("'", "", gsub(" ", "_", tolower(name)))) %>%
  group_by(player, year) %>%
  spread(name, attempts)

