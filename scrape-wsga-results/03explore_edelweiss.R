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

p1 <- fit1_info %>%
  mutate(term = substring(term, 5, 999)) %>%
  inner_join(course2, by = c('term' = 'hole')) %>%
  ggplot(aes(reorder(term, estimate),estimate))+
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5)+
  geom_point(aes(colour = factor(par)), size = 2)+
  coord_flip()

my_score <- data_frame(
  hole = paste0('hole_', 1:18),
  score = c(5,4,5,3,4,5,2,5,5,
            4,5,4,3,4,5,6,4,5)
) %>%
  inner_join(course2) %>%
  mutate(score_rel_to_par = score - par)

p1 +
  geom_point(data = my_score,
             aes(x = hole, y = score_rel_to_par),
             size = 2)
shots_gained <- predict(fit1, newdata = my_score) - my_score$score_rel_to_par

plot()
lowes()

results_long3 <- results_long2 %>%
  mutate(fitted = augment(fit1, newdata = results_long2)$.fitted)
