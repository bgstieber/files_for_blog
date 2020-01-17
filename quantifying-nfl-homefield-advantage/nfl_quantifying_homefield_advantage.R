library(tidyverse)
library(data.table)

data_link <- "https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv"

nfl_data <- fread(data_link)


nfl_data2 <- nfl_data %>%
  as_tibble() %>%
  filter(neutral == 0) %>%
  mutate(home_away_elo_diff = elo1_pre - elo2_pre,
         home_away_prob_diff = elo_prob1 - elo_prob2,
         home_away_qb_elo_diff = qb1_value_pre - qb2_value_pre,
         home_away_score_diff = score1 - score2)




model_data <- nfl_data2 %>%
  filter(playoff == "",
         !is.na(home_away_elo_diff),
         !is.na(home_away_qb_elo_diff),
         !is.na(home_away_score_diff)) %>%
  mutate(season_decade = 10 * floor(season / 10))

model_data %>%
  ggplot(aes(home_away_elo_diff, home_away_score_diff,
             colour = season_decade))+
  geom_jitter(alpha = 0.8)

fit1 <- lm(home_away_score_diff ~ home_away_elo_diff + home_away_qb_elo_diff,
           data = model_data)

fit1_add_sq <- update(fit1, .~. + I(home_away_elo_diff ^ 2) +
                        I(home_away_qb_elo_diff ^ 2))
