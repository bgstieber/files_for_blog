library(tidyverse)
library(scales)
theme_set(theme_bw())

results <- read_csv('data//all_oaks_data.csv') %>%
  mutate(tournament = str_extract(url, 'wsga[0-9]{3,}'))

course_info <- read_csv('data//course_info.csv') %>%
  mutate(tournament = str_extract(url, 'wsga[0-9]{3,}'))

course_info2 <- course_info %>%
  select(hole = `hole#`,
         par,
         yards,
         tournament) %>%
  filter(hole != 'Totals') %>%
  distinct() %>%
  mutate(hole = paste0('hole_', hole))



results_long <- results %>%
  gather(hole, score, -title, -url, -tournament) %>%
  inner_join(course_info2) %>%
  mutate(score_rel_to_par = score - par)

total_results <- results_long %>%
  group_by(title, url, tournament) %>%
  summarise(total_score = sum(score),
            total_rel_to_par = sum(score_rel_to_par)) %>%
  ungroup() %>%
  mutate(score_percentile = ecdf(total_score)(total_score))

results_long <- results_long %>%
  inner_join(total_results) %>%
  mutate(top_25 = ifelse(score_percentile <= .25, 'Y', 'N'))

# average score relative to par by hole
results_long %>%
  group_by(hole, par) %>%
  summarise(avg_score_rel_to_par = mean(score_rel_to_par)) %>%
  ggplot(aes(reorder(hole, avg_score_rel_to_par), avg_score_rel_to_par))+
  geom_col(aes(fill = factor(par)))+
  geom_hline(aes(yintercept = mean(results_long$score_rel_to_par)))+
  coord_flip()


avg_by_hole_and_top_25 <- results_long %>%
  group_by(hole, par, top_25) %>%
  summarise(avg_rel_to_par = mean(score_rel_to_par))


avg_by_hole_and_top_25 %>%
  ggplot(aes(reorder(hole, avg_rel_to_par), avg_rel_to_par))+
  geom_point(aes(colour = top_25))+
  coord_flip()

count_by_score_type <- results_long %>%
  mutate(score_coded = ifelse(score_rel_to_par < 0, 'Birdie or Better',
                              ifelse(score_rel_to_par == 0, 'Par',
                                     ifelse(score_rel_to_par == 1, 'Bogey',
                                            'Double or Worse')))) %>%
  count(hole, par, score_coded) %>%
  group_by(hole) %>%
  mutate(perc_n = n / sum(n))


count_by_score_type %>%
  mutate(hole_n = as.numeric(gsub('hole_', '', hole, fixed = TRUE))) %>%
  ggplot(aes(hole_n, score_coded, fill = n))+
  geom_tile(colour = 'black')+
  geom_text(aes(label = percent(perc_n)))+
  coord_flip()+
  scale_x_reverse(breaks = 1:18)+
  scale_y_discrete(limits = c('Birdie or Better', 'Par', 'Bogey', 'Double or Worse'))+
  scale_fill_viridis_c()


results_long %>%
  group_by(tournament, hole, yards, par) %>%
  summarise(avg_rel_to_par = mean(score_rel_to_par)) %>%
  ggplot(aes(tournament, avg_rel_to_par))+
  geom_jitter(aes(colour = factor(par)),
              position = position_jitter(width = 0.1, height = 0))


# basic modeling
results_wide <- results_long %>%
  select(url, tournament, hole, score_rel_to_par,
         total_rel_to_par, score_percentile) %>%
  spread(hole, score_rel_to_par) %>%
  mutate(front_9 = rowSums(.[paste0('hole_', 1:9)]),
         back_9 = rowSums(.[paste0('hole_', 10:18)])) 

results_wide2 <- results_wide %>%
  mutate_at(vars(starts_with('hole')), 
            .funs = function(x) as.numeric(x <= 0)) %>%
  mutate(top25_indicator = as.numeric(score_percentile <= 0.25))

cor(results_wide2$front_9, results_wide2$back_9)

top25_model <- glm(top25_indicator ~ .,
                   data = select(results_wide2, 
                                 top25_indicator, 
                                 starts_with('hole')),
                   family = 'binomial')

coef_results <- top25_model %>%
  broom::tidy() %>%
  bind_cols(top25_model %>% broom::confint_tidy()) %>%
  left_join(course_info2 %>% 
              distinct(hole, par),
            by = c('term' = 'hole'))


coef_results %>%
  filter(!is.na(par)) %>%
  ggplot(aes(reorder(term, estimate), estimate))+
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 alpha = 0.5)+
  geom_point(aes(colour = factor(par)), size = 2)+
  coord_flip()+
  xlab('hole')+
  ylab('coefficient from model')

yvar <- sapply(2:18, FUN = function(x) paste0('hole_', x:18, '+',
                                              collapse = ''))

yvar <- substring(yvar, 1, nchar(yvar) - 1)

yvar <- paste0('I(', yvar, ')')

xvar <- sapply(1:17,
               FUN = function(x) paste0('hole_', x:1, '+', collapse = ''))

xvar <- substring(xvar, 1, nchar(xvar) - 1)

xvar2 <- paste0('I(', xvar, ')')

model_formulas <- data_frame(yvar, xvar, xvar2) %>%
  mutate(formula1 = paste0(yvar, '~', xvar),
         formula2 = paste0(yvar, '~', xvar2)) 


individual_holes <- lapply(model_formulas$formula1,
                           FUN = function(f) lm(f, data = results_wide))

sum_holes <- lapply(model_formulas$formula2,
                           FUN = function(f) lm(f, data = results_wide))


results_long %>%
  group_by(url, tournament, top_25) %>%
  summarise(sdev_rel_to_par = sd(score_rel_to_par),
            avg_rel_to_par = mean(score_rel_to_par)) %>%
  ggplot(aes(1, sdev_rel_to_par))+
  geom_jitter(aes(colour = top_25),
              position = position_jitter(height = 0))+
  xlim(c(0, 2))
