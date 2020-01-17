library(tidyverse)
library(data.table)
library(ggridges)
library(scales)

theme_set(theme_bw())

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

# visualize distribution of outcomes
model_data %>%
  ggplot(aes(home_away_score_diff, factor(season_decade)))+
  geom_density_ridges(aes(fill = season_decade))+
  scale_fill_viridis_c()+
  scale_x_continuous("Home Score - Away Score",
                     breaks = seq(-100, 100, 10))+
  ylab("Decade")+
  ggtitle("Home vs Away Score Differential by Decade")
# most frequently occurring differentials
model_data %>%
  count(season_decade, home_away_score_diff) %>%
  group_by(season_decade) %>%
  mutate(perc_of_games = n / sum(n)) %>%
  top_n(2, perc_of_games) %>%
  arrange(desc(season_decade), desc(n))

# example model where intercept isn't really interprettable
ggplot(mtcars, aes(wt * 1000, mpg))+
  geom_point()+
  stat_smooth(method = 'lm')+
  scale_x_continuous("Weight (pounds)", labels = comma)+
  ylab("MPG")+
  ggtitle("Predicting Car's MPG with Weight, Using mtcars Data")

mpg_by_wt <- lm(mpg ~ wt, data = mtcars)

# if the weight of the car were zero...?
summary(mpg_by_wt)

model_data %>%
  ggplot(aes(home_away_elo_diff, home_away_score_diff,
             colour = season_decade))+
  geom_jitter(alpha = 0.8)

fit1 <- lm(home_away_score_diff ~ home_away_elo_diff + home_away_qb_elo_diff,
           data = model_data)

fit1_add_sq <- update(fit1, .~. + I(home_away_elo_diff ^ 2) +
                        I(home_away_qb_elo_diff ^ 2))










