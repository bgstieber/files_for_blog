library(tidyverse)

file_names <- dir("data//")

file_groups <- file_names %>%
  gsub(pattern = "_[0-9]{4}.[a-z]{3}",
       replacement = "",
       x = .) %>%
  unique()

file_paths <- file.path("data", dir("data/"))

all_pga_data <- file_groups %>%
  map(~file_paths[grepl(pattern = .x, x = file_paths)]) %>%
  set_names(file_groups) %>%
  map_df(~as_tibble(.x), .id = 'group') %>%
  rowwise() %>%
  mutate(data = map(value, read_csv))


