library(tidyverse)
library(tidycensus)
library(ggmap)
library(scales)
library(maps)
library(readxl)
library(httr)


# lat lon data for plotting
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
  inner_join(map_data('county')) %>%
  mutate(county_fips = ifelse(nchar(fips) == 4,
                              paste0("0", fips), fips))

# download covid data from nyt
nyt_c19_1 <- "https://github.com/nytimes/covid-19-data"
nyt_c19_2 <- "/blob/master/us-counties.csv?raw=true"

nyt_c19 <- data.table::fread(paste0(nyt_c19_1, nyt_c19_2),
                             colClasses = c(fips = "character")) %>%
  filter(date == "2020-11-01") %>%
  select(county_fips = fips, cases)

# get election results from github repo
u1 <- "https://github.com/tonmcg/"
u2 <- "US_County_Level_Election_Results_08-20/raw/master/"
u3 <- "2020_US_County_Level_Presidential_Results.csv"
u4 <- "US_County_Level_Presidential_Results_08-16.csv"

gh_url <- paste0(u1, u2, u3)

e2020.base <- read_csv(gh_url)

e2020 <- e2020.base %>%
  filter(!(state_name %in% c("Alaska", "Hawaii",
                             "District of Columbia")))

e_08_16 <- read_csv(paste0(u1, u2, u4)) %>%
  select(county_fips = fips_code,
         dem_2012, gop_2012, oth_2012, total_2012,
         dem_2016, gop_2016, oth_2016, total_2016)



# get census data using tidycensus
census_data <- get_acs('county', 
                       c(pop_25 = 'B15003_001', 
                         pop_total = "B01001_001",
                         pop_white = "B01001A_001",
                         edu = 'B16010_028', 
                         inc = 'B21004_001')) %>%
  select(-moe) %>%
  spread(variable, estimate) %>%
  mutate(pct_white = pop_white / pop_total,
         pct_edu = edu/pop_25) %>%
  select(-NAME, -edu, -pop_25) %>%
  rename(county_fips = GEOID)

# get land area
la1 <- "https://www2.census.gov/library/publications/2011/"
la2 <- "compendia/usa-counties/excel/LND01.xls"

url1 <- paste0(la1, la2)
GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
land_area.base <- read_excel(tf)
unlink(tf)

land_area <- land_area.base %>%
  select(county_fips = STCOU,
         area_miles_squared = LND110210D)

# put the data together
full_data <- e2020 %>%
  inner_join(census_data) %>%
  inner_join(land_area) %>%
  inner_join(nyt_c19) %>%
  inner_join(e_08_16) %>%
  mutate(pop_dens = pop_total / area_miles_squared,
         case_rate_per_1k = 1000 * (cases/pop_total))


# data work, column creation, then output a final master data set
model_data <- full_data %>%
  inner_join(tibble(state_name = state.name, state_abbr = state.abb)) %>%
  select(-cases, -area_miles_squared, -pop_total, -pop_white,
         -total_votes, -diff, -per_gop, -per_dem, -per_point_diff) %>%
  mutate_at(c("oth_2012", "dem_2012", "gop_2012"),
            ~ . / total_2012) %>%
  mutate_at(c("oth_2016", "dem_2016", "gop_2016"),
            ~ . / total_2016) %>%
  mutate(dem_shift_16_12 = dem_2016 - dem_2012,
         gop_shift_16_12 = gop_2016 - gop_2012,
         oth_shift_16_12 = oth_2016 - oth_2012) %>%
  mutate(fips_n = as.numeric(county_fips)) %>%
  mutate(dem_2016_2party_share = dem_2016 / (gop_2016 + dem_2016),
         nonwhite_pct = 1 - pct_white) %>%
  na.omit()


simple_model <- glm(cbind(votes_dem, votes_gop) ~ 
                      I(log(pop_dens))+
                      I(log(inc))+
                      I(log(case_rate_per_1k))+
                      pct_edu+
                      pct_white+
                      dem_2016+
                      dem_shift_16_12+
                      state_abbr,
                    data = model_data,
                    family = 'binomial',
                    na.action = na.exclude)

# REMEMBER: resid = actual - pred
# starr county: https://www.texastribune.org/2020/11/13/south-texas-voters-donald-trump/
model_data$actual <- model_data$votes_dem / (model_data$votes_gop + model_data$votes_dem)
model_data$pred <- predict(simple_model, type = "response")
model_data$actual_minus_pred <- model_data$actual - model_data$pred


model_data %>%
  inner_join(county_map_with_fips) %>%
  ggplot(aes(long, lat, group = group, fill = actual_minus_pred))+
  geom_polygon(colour = rgb(0.9,0.9,0.9, .1))+
  geom_polygon(data = map_data('state'), fill = NA, colour = 'white')+
  coord_map()+
  scale_fill_gradient2()
