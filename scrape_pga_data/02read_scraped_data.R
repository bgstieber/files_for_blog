library(tidyverse)

all_pga_data <- file.path("data", dir("data/")) %>%
  map(read_csv)



