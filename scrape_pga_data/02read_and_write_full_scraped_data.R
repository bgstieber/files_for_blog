library(tidyverse)

fix_money <- function(data){
  data %>%
    mutate_at(vars(contains("OFFICIAL MONEY WON")),
              as.character) %>%
    mutate_at(vars(contains("POTENTIAL MONEY")),
              as.character)
}

file_names <- dir("data//") 

file_names <- file_names[!grepl("FULL.csv", file_names)]

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

pga1 <- all_pga_data %>%
  filter(group != "pct_total_money_won") %>%
  split(.$group) %>%
  map(~bind_rows(.x$data))


pga2 <- all_pga_data %>%
  filter(group == "pct_total_money_won") %>%
  .$data %>%
  map(fix_money) %>%
  bind_rows()

pga1$pct_total_money_won <- pga2


# map2(names(pga1), pga1, 
#      ~write_csv(.y, paste0("data//", .x, "_FULL.csv")))

for(i in 1:length(pga1)){
  write_csv(pga1[[i]], 
            paste0("data//", names(pga1)[[i]] , "_FULL.csv"))
}