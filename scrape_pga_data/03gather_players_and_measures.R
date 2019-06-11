library(tidyverse)

full_data_files <- dir("data")[grepl("_FULL", dir("data"))]

full_data <- paste0("data//", full_data_files) %>%
  map(~read_csv(.x)) %>%
  set_names(full_data_files)

measure_and_column <- c("avg_driv_dist", 
                        "gir_pct", 
                        "pct_total_money_won", 
                        "sg_app_green", 
                        "sg_around_green", 
                        "sg_off_tee", 
                        "sg_putting", 
                        "sg_tee_to_green", 
                        "total_driving")

money <- full_data$pct_total_money_won_FULL.csv %>%
  mutate(money_won = parse_number(`OFFICIAL MONEY WON`))

money_scaled <-money %>%
  select(year, `PLAYER NAME`, money_won, EVENTS) %>%
  group_by(year) %>%
  mutate(scaled_money = scale(log1p(money_won))) %>%
  ungroup()

sg_tee_to_green <- full_data$sg_tee_to_green_FULL.csv

sg_tee_to_green_minimal <- sg_tee_to_green %>%
  select(year, `PLAYER NAME`,
         'SG:TTG' = AVERAGE, `SG:OTT`, `SG:APR`,`SG:ARG`)

sg_putting <- full_data$sg_putting_FULL.csv

sg_putting_minimal <- sg_putting %>%
  select(year, `PLAYER NAME`, 'SG:PUTT' = AVERAGE)

yoy_performance <- sg_tee_to_green %>%
  select(year, `PLAYER NAME`, AVERAGE) %>%
  arrange(year) %>%
  group_by(`PLAYER NAME`) %>%
  mutate(last_year = lag(year),
         last_year_avg = lag(AVERAGE)) %>%
  filter((last_year + 1) == year) %>%
  ungroup()

yoy_performance %>%
  mutate(change_from_last_year = (AVERAGE - last_year_avg)) %>%
  arrange(desc(abs(change_from_last_year))) %>%
  top_n(25, abs(change_from_last_year))



money_and_sg <- money_scaled %>%
  inner_join(sg_tee_to_green_minimal,
             by = c("year", "PLAYER NAME")) %>%
  inner_join(sg_putting_minimal,
             by = c("year", "PLAYER NAME"))

fit1 <- lm(scaled_money ~ -1 + bs(`SG:TTG`, df = 4), data = money_and_sg)


money_and_sg$pred <- predict(fit1)
