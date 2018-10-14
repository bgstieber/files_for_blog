library(tidyverse)
library(scales)
library(lubridate)

zillow_file_condo <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_Condominum.csv"
zillow_file_single_family <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_SingleFamilyResidence.csv"

zhvi_condos <- zillow_file_condo %>%
  read_csv() 

zhvi_homes <- zillow_file_single_family %>%
  read_csv()
# using https://stackoverflow.com/a/1995984/5619526
monnb <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon 
  } 

mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }


zillow_data_tidy <- zhvi_condos %>%
  gather(date, zhvi, -RegionID, -RegionName, -City, -State,
         -Metro, -CountyName, -SizeRank) %>%
  mutate(date = as.Date(paste0(date, '-01'), '%Y-%m-%d'))

zhvi_homes_tidy <- zhvi_homes %>%
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


zillow_data_tidy %>%
  filter(CountyName == 'Dane') %>%
  filter(year(date) >= 2013) %>%
  ggplot(aes(date, zhvi))+
  geom_line(aes(group = RegionID), alpha = 0.4)+
  stat_smooth()

# when was lowest zhvi since recession
zillow_data_tidy %>%
  filter(CountyName == 'Dane') %>%
  filter(year(date) >= 2009) %>%
  group_by(RegionID) %>%
  slice(which.min(zhvi))

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

## growth rates for Dane county for condos and homes
dane_condos <- zillow_data_tidy %>%
  filter(CountyName == 'Dane')

dane_houses <- zhvi_homes_tidy %>%
  filter(CountyName == 'Dane')

dane_property <- dane_condos %>%
  inner_join(select(dane_houses, RegionID, date, zhvi), 
             by = c('RegionID', 'date'), 
             suffix = c('_condo', '_home'))

dane_property_since_2013 <- dane_property %>%
  filter(year(date) >= 2013) %>%
  arrange(date) %>%
  group_by(RegionID) %>%
  mutate(pct_change_condo = (zhvi_condo - lag(zhvi_condo)) / lag(zhvi_condo),
         pct_change_home = (zhvi_home - lag(zhvi_home)) / lag(zhvi_home))


dane_property_since_2013 %>%
  group_by(RegionName, City) %>%
  summarise_at(vars(starts_with('pct')), mean, na.rm = TRUE) %>%
  gather(variable, value, starts_with('pct')) %>%
  ggplot(aes(reorder(RegionName, value), value, fill = variable))+
  geom_col(position = 'dodge', colour = 'black')+
  coord_flip()

## growth rates for Dane county for condos and homes

all_property <- zillow_data_tidy %>%
  inner_join(select(zhvi_homes_tidy, RegionID, date, zhvi), 
             by = c('RegionID', 'date'), 
             suffix = c('_condo', '_home'))

all_property_since_2013 <- all_property %>%
  filter(year(date) >= 2013) %>%
  arrange(date) %>%
  group_by(RegionID) %>%
  mutate(pct_change_condo = (zhvi_condo - lag(zhvi_condo)) / lag(zhvi_condo),
         pct_change_home = (zhvi_home - lag(zhvi_home)) / lag(zhvi_home)) %>%
  mutate(months_since_jan1_2013 = mondf('2013-01-01', date))


all_property_since_2013_summary <- all_property_since_2013  %>%
  group_by(RegionName, City, State) %>%
  summarise_at(vars(starts_with('pct')), mean, na.rm = TRUE) 

all_property_since_2013_summary %>%
  ggplot(aes(pct_change_condo, pct_change_home))+
  geom_point(alpha = 0.1)+
  geom_abline(aes(slope = 1, intercept = 0))

# nest by zip code and fit models
fit_mod <- function(df){
  lm(I(log(zhvi_condo)) ~ months_since_jan1_2013, data = df)
}

all_property_since_2013_nested <- all_property_since_2013 %>%
  group_by(RegionName, City, CountyName, State) %>%
  nest() %>%
  mutate(model = map(data, fit_mod))

all_property_since_2013_nested_glance <- 
  all_property_since_2013_nested %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE)





