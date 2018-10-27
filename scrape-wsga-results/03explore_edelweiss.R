library(tidyverse)

results <- read_csv("data//edelweiss_data.csv")
course <- read_csv("data//edelweiss_course_data.csv")

results_long <- results %>%
  gather(hole, score, starts_with('hole'))


course2 <- course %>%
  select(hole = `hole#`,
         par,
         yards) %>%
  filter(hole != 'Totals') %>%
  distinct() %>%
  mutate(hole = paste0('hole_', hole))

results_long2 <- results_long %>%
  inner_join(course2) %>%
  mutate(score_to_par = score - par)

results_long2 %>%
  group_by(hole) %>%
  summarise(score_to_par = mean(score_to_par))


results_long2 %>%
  group_by(hole, par) %>%
  summarise(score_to_par = mean(score_to_par)) %>%
  ggplot(aes(reorder(hole, score_to_par), score_to_par))+
  geom_col(aes(fill = factor(par)))+
  coord_flip()

# two holes played ~even par
fit1 <- lm(score_to_par ~ -1 + hole, data = results_long2)
summary(fit1)