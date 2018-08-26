library(tidyverse)
library(lubridate)

theme_set(theme_bw())

daylight_data <- read_csv('..//data//full_daylight_data.csv')

us_data <- map_data('state') %>%
  filter(! region %in% c('alaska', 'hawaii')) %>%
  inner_join(data_frame(region = tolower(state.name), state = state.abb))

# contiguous US only

# from https://www.timeanddate.com/calendar/aboutseasons.html:
# Spring runs from March 1 to May 31;
# Summer runs from June 1 to August 31;
# Fall (autumn) runs from September 1 to November 30; and
# Winter runs from December 1 to February 28
daylight_data_contig <- daylight_data %>%
  filter(! state %in% c('AK', 'HI')) %>%
  mutate(season = ifelse(month(date) %in% c(12, 1, 2),
                         'Winter',
                         ifelse(month(date) %in% 3:5,
                                'Spring',
                                ifelse(month(date) %in% 6:8, 
                                       'Summer', 'Fall'))))


daylight_summary_state_season <- daylight_data_contig %>%
  group_by(state, season) %>%
  summarise(total_daylight = sum(daylight_duration),
            avg_daylight = mean(daylight_duration),
            median_daylight = median(daylight_duration),
            q25_daylight = quantile(daylight_duration, 0.25),
            q75_daylight = quantile(daylight_duration, 0.75),
            count_days = n())

daylight_data_contig_with_location <- us_data
  inner_join(daylight_data_contig)


daylight_summary_state_season_location <- us_data %>%
  inner_join(daylight_summary_state_season)


daylight_data_contig %>%
  mutate(daylight_duration = daylight_duration / 60) %>%
  ggplot(aes(date,
             reorder(state, daylight_duration),
             fill = daylight_duration))+
  geom_tile()+
  scale_fill_viridis_c('Daylight (hours)')+
  theme_minimal()+
  theme(panel.grid = element_blank())+
  xlab('')+ylab('')+
  ggtitle('Daily Daylight by State')


daylight_summary_state_season_location %>%
  mutate(season = factor(season, levels = c('Winter', 'Spring', 
                                            'Summer', 'Fall'))) %>%
  group_by(season) %>%
  mutate(avg_daylight_scaled = scale(avg_daylight)) %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = avg_daylight_scaled),
               colour = 'black')+
  facet_wrap(~season)+
  scale_fill_gradient2(breaks = -1:1,
                       name = 'Difference from Seasonal Average',
                       labels = c('-1 SD', 'Avg', '+1 SD'),
                       low = '#5e3c99',
                       high = '#e66101')+
  theme_minimal()+
  theme(axis.text = element_blank(),
        panel.grid = element_blank())+
  xlab('')+ylab('')+
  coord_map()+
  ggtitle('Difference in Overall Seasonal Average Daylight by State and Season',
          subtitle = paste0('States colored orange have higher average daylight',
                            ' durations during that season,\nwhereas states ',
                            'colored purple experience shorter than average',
                            ' daylight during that season.'))


daylight_data_contig_weekly_summary <- daylight_data_contig %>%
  mutate(week_date = week(date)) %>%
  group_by(state, week_date) %>%
  summarise(first_date = min(date),
            avg_duration = mean(daylight_duration)) %>%
  inner_join(us_data)

