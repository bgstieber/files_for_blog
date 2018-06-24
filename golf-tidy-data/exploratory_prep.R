library(tidyverse)
library(scales)
library(lme4)
theme_set(theme_bw())

gh <- "https://raw.githubusercontent.com/bgstieber/files_for_blog/master/golf-tidy-data/data"

tidy_scores <- read_csv(paste0(gh, '/tidy_golf_scores.csv')) %>%
  filter(tot <= 120)

untidy_scores <- read_csv(paste0(gh, '/untidy_golf_scores.csv')) %>%
  filter(tot <= 120)

head(tidy_scores)

# plot1: count of scores by hole

tidy_scores %>%
  mutate(rel_to_par2 = 
           ifelse(rel_to_par <= -1, "Birdie or Better",
                  ifelse(rel_to_par == 0, "Par",
                         ifelse(rel_to_par == 1, "Bogey", 
                                "Double or Worse")))) %>%
  mutate(rel_to_par2 = factor(rel_to_par2, 
                              levels = c("Birdie or Better",
                                         "Par", "Bogey",
                                         "Double or Worse"))) %>%
  count(hole, rel_to_par2) %>%
  group_by(hole) %>%
  mutate(perc_n = n / sum(n)) %>%
  ggplot(aes(factor(hole, 18:1), rel_to_par2))+
  geom_tile(aes(fill = n), colour = 'black')+
  geom_text(aes(label = percent(perc_n)),
            size = 5)+
  coord_flip()+
  scale_fill_distiller(palette = 'OrRd', direction = 1,
                       name = 'Count')+
  xlab('Hole')+
  ylab('Score Relative to Par')+
  ggtitle('Heatmap of Scores Relative to Par by Hole',
          subtitle = paste0('Cells are colored according to count',
                            ' of participants with that score for the hole.\n',
                            'Percentages within each cell show the frequency of',
                            'participants with that score for the hole.'))+
  theme(panel.grid = element_blank())+
  theme_minimal()


# plot2: hardest hole?
tidy_scores %>%
  group_by(hole, par) %>%
  summarise(avg_rel_to_par = mean(rel_to_par)) %>%
  ggplot(aes(reorder(hole, avg_rel_to_par), avg_rel_to_par,
             fill = factor(par)))+
  geom_col(colour = 'black')+
  geom_hline(aes(yintercept = mean(tidy_scores$rel_to_par)),
             linetype = 'dashed', colour = 'blue', size = 1.2)+
  coord_flip()+
  xlab('Hole (ordered by difficulty)')+
  ylab('Average Relative to Par')+
  scale_fill_brewer(name = 'Par', palette = 'Greens')+
  ggtitle('Average Score Relative to Par by Hole',
          subtitle = 
            paste0('Holes are ordered along the vertical axis by difficulty.\n',
                   'Blue dashed line is the overall average score relative to',
                   ' par for across all holes.'))


# plot3: another way to look at hardest hole
# lollipop chart in style of julia silge
# https://juliasilge.com/blog/gender-pronouns/
# plot3: another way to look at hardest hole
# lollipop chart in style of julia silge
# https://juliasilge.com/blog/gender-pronouns/
tidy_scores %>%
  group_by(hole, par) %>%
  summarise(avg_rel_to_par = mean(rel_to_par)) %>%
  ungroup() %>%
  mutate(tot_avg = mean(avg_rel_to_par),
         diff_from_avg = avg_rel_to_par - tot_avg) %>%
  mutate(diff_coded = ifelse(diff_from_avg < 0,
                             'Easier than Average',
                             'Harder than Average')) %>%
  ggplot(aes(reorder(hole, diff_from_avg), diff_from_avg,
             colour = diff_coded))+
  geom_segment(aes(xend = reorder(hole, diff_from_avg), y = 0, yend = diff_from_avg),
               size = 1.2)+
  geom_point(size = 3)+
  scale_color_brewer(palette = 'Set2', name = '')+
  xlab('Hole (ordered by difficulty)')+
  ylab('Difference in Score Relative to Par versus Average')+
  ggtitle('Average Score on Hole Relative to Par Versus Overall Course Average',
          subtitle = 
            paste0('Holes with negative values are, on average easier. ',
                   'Holes with positive values are more difficult.'))

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

# plot4, estimated difficult for each hole by tournament type
# getting a little more precise about quantifying difficulty
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
## plot5
## kind of a simplified qq plot
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

dummy_preds <- cbind(dummy_table2,
                     predict(fit2, 
                             newdata = dummy_table2, 
                             interval = 'prediction'))

dummy_preds %>%
  ggplot(aes(reorder(hole_f, fit), 
             fit, ymin = lwr, ymax = upr, colour = broke_90))+
  geom_pointrange(position = position_dodge(width = 1))+
  coord_flip()

# plot 6 where do the top performers perform better than the worst
dummy_preds  %>%
  select(hole_f, broke_90, fit) %>%
  spread(broke_90, fit) %>%
  mutate(diff_avg = `Did not Break 90` - `Broke 90`) %>%
  ggplot(aes(reorder(hole_f, -diff_avg), diff_avg))+
  geom_col()+
  xlab('Hole (ordered by average difference)')+
  ylab('Average Stroke Improvement from Top 30% to Bottom 70%')+
  ggtitle('Where do the Better Golfers Shine?',
          subtitle = paste0('The golfers finishing in the top 30% tended to',
                            ' perform better on the more difficult holes.'))

tidy_scores %>%
  arrange(name) %>%
  group_by(name) %>%
  ggplot(aes(hole, rel_to_par, colour = factor(tourn_year)))+
  stat_summary(position = position_dodge(width = 1))+
  facet_wrap(~tourn_year)
