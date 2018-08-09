library(tidyverse)
theme_set(theme_bw())


remove_empty_strings <- function(x) x[x != '']

data_url <- function(year=2017, state, place){
  paste0("http://aa.usno.navy.mil/cgi-bin/aa_durtablew.pl?form=1&year=",
         year, "&task=-1&",
         "state=",state, "&place=",place)
}

get_time_zone <- function(data){
  data[12] %>%
    strsplit("  ") %>%
    unlist %>%
    trimws %>%
    remove_empty_strings() %>%
    .[2]
}

get_daylight_time <- function(d){
  
  d1 <- d %>%
    as.character() %>%
    strsplit(":") 
  
  
  sapply(d1, FUN = function(x) 60 * as.numeric(x[1]) + as.numeric(x[2]))
  
}

get_daylight_data <- function(data, year = 2017){
  
  data[20:50] %>%
    paste(collapse = '\n') %>%
    read_fwf(col_positions = fwf_widths(c(8, rep(9, 11), NA))) %>%
    set_names(c('day', month.abb)) %>%
    gather(month, daylight, -day) %>%
    mutate(date = as.Date(paste0(day, month, year),
                          '%d%b%Y')) %>%
    filter(!is.na(date)) %>%
    mutate(daylight_duration = get_daylight_time(daylight))
  
}

# get data
## state capital csv
states_capital <- read_csv("https://gist.githubusercontent.com/mbostock/9535021/raw/902265051e775cd35a6aa6307a23070f739ebed1/us-state-capitals.csv") %>%
  mutate(description = tolower(gsub(pattern = "<br>", 
                                    replacement = "", 
                                    x = description,
                                    fixed = TRUE))) %>%
  inner_join(data_frame(name = state.name,
                        abbr = state.abb))
## all daylight data (initially just a list of character vectors)
full_data_2017 <- mapply(x = states_capital$abbr, 
                         y = states_capital$description,
                         FUN = function(x, y){
                           readLines(data_url(state = x,place = y))
                           },
                         SIMPLIFY = FALSE)
# add time zones onto state data                  
time_zone_data <- stack(lapply(full_data_2017, get_time_zone)) %>%
  select('time_zone' = values,
         'abbr' = ind) %>%
  inner_join(states_capital)
# get daylight data for each state
daylight_list <- lapply(full_data_2017, get_daylight_data)

full_daylight_data <- do.call('rbind',
                              mapply(names(daylight_list), 
                                     daylight_list, 
                                     FUN = function(x, y) 
                                       y %>% 
                                        mutate(state = x), 
                                     SIMPLIFY = FALSE))


write_csv(time_zone_data, "..//data//time_zone_data.csv")

write_csv(full_daylight_data, "..//data//full_daylight_data.csv")


full_daylight_data2 <- full_daylight_data %>%
  filter(! state %in% c('AK', 'HI')) %>%
  mutate(scale_daylight = scale(daylight_duration)) 

full_daylight_data2 %>%
  ggplot(aes(date, reorder(state, scale_daylight), fill = scale_daylight))+
  geom_raster()+
  scale_fill_gradient2()

total_daylight_by_state <- full_daylight_data2 %>%
  group_by(state) %>%
  summarise(total_daylight_hours = sum(daylight_duration)) %>%
  inner_join(time_zone_data, by = c('state' = 'abbr'))

total_daylight_by_state %>%
  ggplot(aes(reorder(state, total_daylight_hours), total_daylight_hours))+
  geom_point()+
  coord_flip()



map_data('state') %>%filter(! region %in% c('alaska', 'hawaii')) %>%
  inner_join(data_frame(region = tolower(state.name), state = state.abb)) %>%
  inner_join(total_daylight_by_state) %>%  
  ggplot(aes(long, lat, fill = total_daylight_hours, group = group))+
  geom_polygon()
