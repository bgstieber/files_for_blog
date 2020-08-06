## original map
## https://www.reddit.com/r/dataisbeautiful/comments/i047e9/oc_countylevel_map_of_maskusage_in_the_united/

# load packages
library(tidyverse)
library(tidycensus)
library(broom)
library(maps)
library(ggmap)
library(scales)

# get 2016 election results
github_raw <- "https://raw.githubusercontent.com/"
repo <- "tonmcg/County_Level_Election_Results_12-16/master/"
data_file <- "2016_US_County_Level_Presidential_Results.csv"
results_16 <- read_csv(paste0(github_raw, repo, data_file)) %>%
  filter(! state_abbr %in% c('AK', 'HI')) %>%
  select(-X1) %>%
  mutate(total_votes = votes_dem + votes_gop,
         trump_ratio_clinton = 
           (votes_gop/total_votes) / (votes_dem/total_votes),
         two_party_ratio = (votes_dem) / (votes_dem + votes_gop),
         trump_pct = votes_gop/total_votes) %>%
  mutate(log_trump_ratio = log(trump_ratio_clinton)) 

# get mask usage
f1 <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master"
f2 <- "/mask-use/mask-use-by-county.csv" 
mask_usage <-  paste0(f1, f2) %>% 
  read_csv() %>%
  mutate(fips_numeric = as.numeric(COUNTYFP)) %>%
  mutate(never_rarely = NEVER + RARELY,
         freq_always = FREQUENTLY + ALWAYS)

# get covid deaths
f1 <- "https://usafactsstatic.blob.core.windows.net/"
f2 <- "public/data/covid-19/covid_deaths_usafacts.csv"

covid_deaths.base <- paste0(f1, f2) %>%
  read_csv()

covid_deaths <- covid_deaths.base %>%
  filter(`County Name` != "Statewide Unallocated") %>%
  select(-`County Name`, - State, -stateFIPS) %>%
  pivot_longer(cols = -countyFIPS,
               names_to = "date",
               values_to = "deaths") %>%
  mutate(date = lubridate::mdy(date))

# get covid cases
f1 <- "https://usafactsstatic.blob.core.windows.net/"
f2 <- "public/data/covid-19/covid_confirmed_usafacts.csv"

covid_cases <- paste0(f1, f2) %>%
  read_csv() %>%
  filter(`County Name` != "Statewide Unallocated") %>%
  select(-`County Name`, - State, -stateFIPS) %>%
  pivot_longer(cols = -countyFIPS,
               names_to = "date",
               values_to = "cases") %>%
  mutate(date = lubridate::mdy(date))

# get county population

f1 <- "https://usafactsstatic.blob.core.windows.net/public"
f2 <- "/data/covid-19/covid_county_population_usafacts.csv"

county_pop <- read_csv(paste0(f1, f2))

# join deaths, cases, and population together
covid_deaths.july_1 <- covid_deaths %>%
  inner_join(covid_cases) %>%
  filter(date == "2020-07-01") %>%
  inner_join(county_pop) %>%
  mutate(deaths_per_100K = (deaths / population) * 100000,
         deaths_per_cases = deaths/(1 + cases))

# get census data
# population, education (bachelors or higher)
census_data <- get_acs('county', 
                       c(pop_25 = 'B15003_001', 
                         edu = 'B16010_041', 
                         inc = 'B21004_001')) %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(fips_numeric = as.numeric(GEOID)) %>%
  mutate(edu_pct = edu / pop_25)
# data on female 65 and older (census breaks it into male/female, just choosing one)
census_age_data <- get_acs("county",
                           c(female = "B01001_026", 
                             female_65_66 = "B01001_044", 
                             female_67_69 = "B01001_045",
                             female_70_74 = "B01001_046", 
                             female_75_79 = "B01001_047", 
                             female_80_84 = "B01001_048",
                             female_85plus = "B01001_049")) %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(females_65_plus_pct = (female_65_66 + female_67_69 + female_70_74 +
                                  female_75_79+female_80_84+female_85plus)/female) %>%
  mutate(fips_numeric = as.numeric(GEOID)) %>%
  select(fips_numeric, females_65_plus_pct)

# merge census data
census_data <- census_data %>% inner_join(census_age_data)

# get population density (from a previous analysis I did)
github_raw <- "https://raw.githubusercontent.com/"
repo <- "bgstieber/files_for_blog/master/election-map/"
data_file <- "Data/pop_density_by_county.csv"

pop_density <- read_csv(paste0(github_raw, repo, data_file), skip = 1) %>%
  mutate(population = as.numeric(Population)) %>%
  select(geography = `Geographic area`,
         FIPS = `Target Geo Id2`,
         population,
         'population_density' = `Density per square mile of land area - Population`) %>%
  select(FIPS, population_density)

# put the data together
model_data.base <- mask_usage %>%
  inner_join(covid_deaths.july_1, 
             by = c("fips_numeric" = "countyFIPS")) %>%
  inner_join(results_16, by = c("fips_numeric" = "combined_fips")) %>%
  inner_join(census_data) %>%
  inner_join(pop_density, by = c("fips_numeric" = "FIPS"))

# create modeling data set
model_data <- model_data.base %>%
  select(COUNTYFP, fips_numeric,
         county_name,
         freq_always,
         deaths_per_100K,
         deaths_per_cases,
         trump_ratio_clinton,
         inc,
         edu_pct,
         population_density,
         females_65_plus_pct,
         state_abbr) %>%
  na.omit()

# straightforward linear model to predict percentage
fitmod <- lm(freq_always ~ 
               I(log1p(deaths_per_100K))+
               I(log(inc))+
               I(log(trump_ratio_clinton))+
               edu_pct+
               I(log1p(females_65_plus_pct))+
               state_abbr +
               I(log(population_density)),
             data = model_data)


special_fips <- c(12091L, 22099L, 37053L, 48167L, 51001L, 
                  53053L, 53055L)

county_map_with_fips <- county.fips %>% 
  separate(polyname, c('region', 'subregion'), ',') %>%
  mutate(subregion = ifelse(fips %in% special_fips,
                            stringi::stri_split_fixed(subregion,
                                                      ":",
                                                      n=1,
                                                      tokens_only = TRUE) %>%
                              unlist,
                            subregion)) %>%
  unique() %>%
  inner_join(map_data('county')) 

# add residuals and fitted
## if residual is positive: 
## actual > predicted (underestimated mask compliance)
## if residual is negative:
## actual < predict (overestimated mask compliance)
model_data.augment <- augment(fitmod, model_data)

# data for map making
map_plot_data <-  model_data.augment %>%
  select(COUNTYFP, fips_numeric, freq_always, 
         fitted = .fitted, 
         resid = .resid, 
         stndrd_resid = .std.resid) %>%
  inner_join(county_map_with_fips, by = c("fips_numeric" = "fips"))

# using a sequential __not__ diverging palette
map_plot_data %>%
  ggplot(aes(long, lat, group = group, fill = freq_always))+
  geom_polygon(colour = rgb(0.9,0.9,0.9, .1))+
  geom_polygon(data = map_data('state'), fill = NA, colour = 'white')+
  scale_fill_distiller(palette = "YlGnBu",
                       name = '% wearing mask frequently/always',
                       direction = 1,
                       labels = percent)+
  theme_minimal()+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'top')+
  coord_map()+
  xlab('')+ylab('')

model_data %>%
  group_by(state_abbr) %>%
  summarise(median_freq_always = median(freq_always)) %>%
  inner_join(tibble(state_abbr = state.abb, region = tolower(state.name))) %>%
  inner_join(map_data("state")) %>%
  ggplot(aes(long, lat, fill = median_freq_always, group = group))+
  geom_polygon(colour = rgb(0.9,0.9,0.9, .1))+
  geom_polygon(data = map_data('state'), fill = NA, colour = 'white')+
  scale_fill_distiller(palette = "YlGnBu",
                       name = 'median % wearing mask frequently/always',
                       direction = 1,
                       labels = percent)+
  theme_minimal()+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'top')+
  coord_map()+
  xlab('')+ylab('')


map_plot_data %>%
  ggplot(aes(long, lat, group = group, fill = resid))+
  geom_polygon(colour = rgb(0.9,0.9,0.9, .1))+
  geom_polygon(data = map_data('state'), fill = NA, colour = 'white')+
  scale_fill_gradientn(paste0("Prediction Error\n(pink overestimates",
                              " mask compliance,\ngreen underestimates",
                              "mask compliance)  "),
                       colours = brewer_pal(palette = 'PiYG')(7),
                       limits = c(-1.05, 1.05) * max(abs(range(map_plot_data$resid))),
                       values = rescale(c(-1, -.4, -.15, 0, .15, .4, 1)))+
  theme_minimal()+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'top')+
  coord_map()+
  xlab('')+ylab('')


augment(fitmod, model_data) %>%
  group_by(state_abbr) %>%
  summarise(median_resid = median(.resid)) %>%
  inner_join(tibble(state_abbr = state.abb, region = tolower(state.name))) %>%
  inner_join(map_data("state")) %>%
  ggplot(aes(long, lat, fill = median_resid, group = group))+
  geom_polygon(colour = rgb(0.9,0.9,0.9, .1))+
  geom_polygon(data = map_data('state'), fill = NA, colour = 'white')+
  scale_fill_gradientn(paste0("Prediction Error\n(pink overestimates",
                              " mask compliance,\ngreen underestimates",
                              "mask compliance)  "),
                       colours = brewer_pal(palette = 'PiYG')(7),
                       values = rescale(c(-1, -.4, -.15, 0, .15, .4, 1)))+
  theme_minimal()+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'top')+
  coord_map()+
  xlab('')+ylab('')
