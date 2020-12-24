library(tidyverse)
library(lubridate)
library(zoo)

roll_sum_7 <- function(x){
  rollsum(x, k = 7, align = "right", na.pad = TRUE)
}

csv_url <- "https://opendata.arcgis.com/datasets/b913e9591eae4912b33dc5b4e88646c5_10.csv?where=GEO%20%3D%20%27County%27&outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
county_pop_url <- "https://www.wisconsin-demographics.com/counties_by_population"

wi_county_pop <- html_table(read_html(county_pop_url))[[1]] %>%
  as_tibble() %>%
  select(-Rank) %>%
  rename(NAME = County) %>%
  mutate(NAME = trimws(gsub("County", "", NAME))) %>%
  mutate(Population = as.numeric(gsub(",", "", Population))) %>%
  filter(!is.na(Population)) %>%
  mutate(pct_of_pop = Population / sum(Population))

full_data.base <- read_csv(csv_url,
                           guess_max = 5000)


# % of tests by date with moving average

full_data <- full_data.base %>%
  mutate(DATE = ymd_hms(DATE)) %>%
  arrange(DATE) %>%
  inner_join(wi_county_pop)



pct_of_tests <- full_data %>%
  arrange(DATE) %>%
  select(DATE, NAME, TEST_NEW, POS_NEW, Population, pct_of_pop) %>%
  filter(month(DATE) >= 6) %>%
  group_by(DATE) %>%
  mutate(total_test_on_date = sum(TEST_NEW),
         total_pos_on_date = sum(POS_NEW)) %>%
  group_by(NAME) %>%
  mutate(rolling_tests = roll_sum_7(TEST_NEW),
         rolling_total_tests = roll_sum_7(total_test_on_date),
         rolling_pos = roll_sum_7(POS_NEW),
         rolling_total_pos = roll_sum_7(total_pos_on_date)) %>%
  mutate(pct_of_total_tests = rolling_tests/rolling_total_tests,
         pct_of_total_pos = rolling_pos/rolling_total_pos) 


top6_counties <- wi_county_pop %>%
  top_n(6, Population) %>%
  pull(NAME)

pct_of_tests %>%
  filter(NAME %in% top6_counties) %>%
  ggplot(aes(DATE,
             pct_of_total_tests - pct_of_pop,
             fill = 0 < pct_of_total_tests - pct_of_pop))+
  geom_col()+
  facet_wrap(~NAME)+
  scale_fill_manual("Higher Test Rate\nthan Population",
                    values = c("FALSE" = "#c5050c", "TRUE" = "grey20"),
                    labels = c("FALSE" = "No", "TRUE" = "Yes"))


pct_of_tests %>%
  filter(NAME %in% top6_counties) %>%
  ggplot(aes(DATE,
             pct_of_total_tests - pct_of_total_pos,
             fill = 0 < pct_of_total_tests - pct_of_total_pos))+
  geom_col()+
  facet_wrap(~NAME)+
  scale_fill_manual("Higher Test Rate\nthan Case Rate",
                    values = c("FALSE" = "#c5050c", "TRUE" = "grey20"),
                    labels = c("FALSE" = "No", "TRUE" = "Yes"))


pct_of_tests %>%
  filter(NAME %in% top6_counties) %>%
  ggplot(aes(DATE,
             rolling_pos/rolling_tests))+
  geom_col()+
  facet_wrap(~NAME)


