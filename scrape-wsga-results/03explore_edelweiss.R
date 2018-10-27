library(tidyverse)
library(broom)
theme_set(theme_bw())
# data manipulation
results <- read_csv("data//edelweiss_data.csv")
course <- read_csv("data//edelweiss_course_data.csv")
# wide to long (tidy)
results_long <- results %>%
  gather(hole, score, starts_with('hole'))
# clean up course information data
course2 <- course %>%
  select(hole = `hole#`,
         par,
         yards) %>%
  filter(hole != 'Totals') %>%
  distinct() %>%
  mutate(hole = paste0('hole_', hole))
# join to get course information
results_long2 <- results_long %>%
  inner_join(course2) %>%
  mutate(score_to_par = score - par)
# average score to par by hole
results_long2 %>%
  group_by(hole, par) %>%
  summarise(score_to_par = mean(score_to_par)) %>%
  ggplot(aes(reorder(hole, score_to_par), score_to_par))+
  geom_col(aes(fill = factor(par)))+
  coord_flip()

# two holes played ~even par
fit1 <- lm(score_to_par ~ -1 + hole, data = results_long2)
summary(fit1)
fit1_info <- tidy(fit1, conf.int = TRUE)

fit1_info %>%
  mutate(term = substring(term, 5, 999)) %>%
  inner_join(course2, by = c('term' = 'hole')) %>%
  ggplot(aes(reorder(term, estimate),estimate))+
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5)+
  geom_point(aes(colour = factor(par)), size = 2)+
  coord_flip()
