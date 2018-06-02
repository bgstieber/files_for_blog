library(tidyverse)
library(scales)
library(lme4)
theme_set(theme_bw())

gh <- "https://raw.githubusercontent.com/bgstieber/files_for_blog/master/golf-tidy-data/data"

tidy_scores <- read_csv(paste0(gh, '/tidy_golf_scores.csv'))

untidy_scores <- read_csv(paste0(gh, '/untidy_golf_scores.csv'))

head(tidy_scores)
# hardest hole?
tidy_scores %>%
  group_by(hole, par) %>%
  summarise(avg_rel_to_par = mean(rel_to_par)) %>%
  ggplot(aes(reorder(hole, avg_rel_to_par), avg_rel_to_par,
             fill = factor(par)))+
  geom_col(colour = 'black')+
  coord_flip()+
  xlab('Hole')+
  ylab('Average Relative to Par')+
  scale_fill_brewer(name = 'Par')


tidy_scores2 <- tidy_scores %>%
  mutate(tourn_year_f = factor(tourn_year),
         hole_f = factor(hole,
                         levels = c(12, 1:11, 13:18)))
## look at past performance to find most difficult holes
simple_mod <- lmer(rel_to_par ~ hole_f + tourn_type +
                     (1|tourn_year_f) + (1|tourn_year_f:name),
                   data = tidy_scores2)

dummy_table <- tidy_scores2 %>%
  select(hole_f, tourn_type) %>%
  unique()

# taken from https://stats.stackexchange.com/a/147837/99673
predFun <- function(fit) {
  predict(fit, dummy_table, re.form = NA)
}

bb <- bootMer(simple_mod,
              nsim = 250,
              FUN = predFun,
              seed = 101)

bootstrapped_preds <- bb$t %>%
  as.data.frame() %>%
  gather(hole, rel_to_par)


dummy_table_boot <- cbind(dummy_table, t(bb$t)) %>%
  gather(iter, rel_to_par, -hole_f, -tourn_type) %>%
  mutate(hole_n = as.numeric(as.character(hole_f)))


ggplot(dummy_table_boot, 
       aes(factor(hole_n, levels = 18:1), rel_to_par, fill = tourn_type))+
  geom_boxplot()+
  coord_flip()+
  facet_wrap(~tourn_type)

ggplot(dummy_table_boot, 
       aes(reorder(hole_n, -rel_to_par), rel_to_par, fill = tourn_type))+
  geom_hline(data = dummy_table_boot %>%
               group_by(tourn_type) %>%
               summarise(rel_to_par = mean(rel_to_par)),
             aes(yintercept = rel_to_par,
                 colour = tourn_type),
             size = 1.2,
             linetype = 'dashed')+
  geom_boxplot()+
  facet_wrap(~tourn_type)+
  scale_fill_brewer(palette = 'Set1')+
  scale_colour_brewer(palette = 'Set1')+
  xlab('Hole (ordered by difficulty)')+
  ylab('Score Relative to Par')

## what separates the top X% from the rest?
seq(0,1,.025) %>%
  map_df(~untidy_scores %>% 
           group_by(tourn_type, 'p' = .x) %>% 
           summarise('perc' = quantile(tot, .x))) %>%
  ggplot(aes(p, perc, colour = tourn_type))+
  geom_point()+
  scale_x_continuous('Percentile', labels = percent)+
  scale_y_continuous(breaks = seq(70, 160, 10),
                     name = 'Final Score')+
  scale_colour_brewer(palette = 'Set1',
                      name = 'Tournament Type')

tidy_scores3 <- tidy_scores2 %>%
  mutate(broke_90 = ifelse(tot < 90, 'Broke 90', 'Did not Break 90'))

fit2 <- lm(rel_to_par ~ hole_f * broke_90, data = tidy_scores3)

dummy_table2 <- tidy_scores3 %>%
  select(hole_f, broke_90) %>%
  unique()

dummy_preds <- cbind(dummy_table2, predict(fit2,
                       newdata = dummy_table2,
                       interval = 'prediction'))

dummy_preds  %>%
  select(hole_f, broke_90, fit) %>%
  spread(broke_90, fit) %>%
  mutate(diff_avg = `Broke 90` - `Did not Break 90`) %>%
  ggplot(aes(reorder(hole_f, diff_avg), diff_avg))+
  geom_point()+
  coord_flip()

