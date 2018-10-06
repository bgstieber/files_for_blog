library(tidyverse)

zillow_file <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_Condominum.csv"

zillow_data <- zillow_file %>%
  read_csv()


zillow_data_tidy <- zillow_data %>%
  gather(date, zhvi, -RegionID, -RegionName, -City, -State,
         -Metro, -CountyName, -SizeRank) %>%
  mutate(date = as.Date(paste0(date, '-01'), '%Y-%m-%d'))


highest_dane_zips <- zillow_data_tidy %>%
  filter(CountyName == 'Dane') %>%
  filter(lubridate::year(date) >= 2016) %>%
  group_by(RegionName) %>%
  summarise(median_zhvi = median(zhvi),
            mean_zhvi = mean(zhvi),
            q25_zhvi = quantile(zhvi, .25),
            q75_zhvi = quantile(zhvi, .75))


highest_hennepin_zips <- zillow_data_tidy %>%
  filter(CountyName == 'Hennepin') %>%
  filter(lubridate::year(date) >= 2016) %>%
  group_by(RegionName) %>%
  summarise(median_zhvi = median(zhvi),
            mean_zhvi = mean(zhvi),
            q25_zhvi = quantile(zhvi, .25),
            q75_zhvi = quantile(zhvi, .75))


highest_dane_zips %>%
  ggplot(aes(reorder(RegionName, median_zhvi), median_zhvi))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(labels = dollar, name = 'Median ZHVI for Condos since 2016')+
  xlab('Dane County ZIP')

highest_hennepin_zips %>%
  ggplot(aes(reorder(RegionName, median_zhvi), median_zhvi))+
  geom_col(aes(fill = RegionName == 55401), colour = 'black')+
  coord_flip()+
  scale_y_continuous(labels = dollar, name = 'Median ZHVI for Condos since 2016')+
  xlab('Hennepin County ZIP')+
  scale_fill_brewer(palette = 'Set1')+
  theme(legend.position = 'none')

zillow_data_tidy %>%
  filter(CountyName == 'Hennepin') %>%
  ggplot(aes(date, zhvi))+
  geom_line(aes(group = RegionName,
                colour = RegionName == 55401),
            size = 2, alpha = 0.5)+
  #stat_smooth(method = 'loess', span = 0.4)+
  scale_colour_brewer(palette = 'Set1')+
  theme(legend.position = 'none')+
  scale_y_continuous(labels = dollar, name = 'Median ZHVI for Condos')
