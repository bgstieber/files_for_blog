library(tidyverse)



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


naive_read <- function(year = 2017, state, place){
  
  dat <- read_fwf(data_url(year, state, place),
                  skip = 19,
                  col_positions = fwf_widths(c(8, rep(9, 12))))
  
  setNames(dat, c('day', month.abb))
  
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
full_data <- mapply(x = states_capital$abbr,
                    y = states_capital$description,
                    FUN = function(x, y){
                      readLines(data_url(state = x,place = y))
                      },
                    SIMPLIFY = FALSE)
# add time zones onto state data                  
time_zone_data <- stack(lapply(full_data, get_time_zone)) %>%
  select('time_zone' = values,
         'abbr' = ind) %>%
  inner_join(states_capital)
