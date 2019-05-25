library(rvest)
library(glue)
library(tidyverse)
library(scales)
library(extrafont)
loadfonts()

theme_bgs <- function(){
  theme_bw() +
    theme(text = element_text(family = 'Segoe UI'),
          plot.title = element_text(face = 'plain', size = 14),
          plot.subtitle = element_text(family = 'Segoe UI Semibold'),
          panel.border = element_rect(colour = 'grey85'),
          panel.grid.minor = element_line(colour = "grey98", size = 0.25),
          axis.title = element_text(family = 'Segoe UI Semibold', size = 12),
          axis.text = element_text(size = 12),
          axis.ticks = element_blank(),
          legend.justification = 'top',
          legend.title = element_text(family = 'Segoe UI Semibold'),
          strip.background = element_rect(fill = 'grey92'),
          strip.text = element_text(family = 'Segoe UI Semibold'))
}

theme_set(theme_bgs())

scrape_pga <- function(url){
  url %>%
    read_html() %>%
    html_table(fill = TRUE) %>%
    .[[2]]
}

ball_striking_urls <- data_frame(years = 2004:2019) %>%
  mutate(total_driving = glue("https://www.pgatour.com/stats/stat.129.{years}.html"),
         sg_tee_to_green = glue("https://www.pgatour.com/stats/stat.02674.{years}.html"),
         sg_off_tee = glue("https://www.pgatour.com/stats/stat.02567.{years}.html"),
         sg_app_green = glue("https://www.pgatour.com/stats/stat.02568.{years}.html"),
         sg_around_green = glue("https://www.pgatour.com/stats/stat.02569.{years}.html"),
         sg_putting = glue("https://www.pgatour.com/stats/stat.02564.{years}.html"),
         gir_pct = glue("https://www.pgatour.com/stats/stat.103.{years}.html"),
         avg_driv_dist = glue("https://www.pgatour.com/stats/stat.101.{years}.html"),
         pct_total_money_won = glue("https://www.pgatour.com/stats/stat.02447.{years}.html"))


sg_around_green <- map2(ball_striking_urls$sg_around_green, 
                   ball_striking_urls$years, 
                   ~ scrape_pga(.x) %>% mutate(year = .y))

sg_around_green <- sg_around_green %>% bind_rows() %>% as_tibble()

sg_putting <- map2(ball_striking_urls$sg_putting, 
                   ball_striking_urls$years, 
                   ~ scrape_pga(.x) %>% mutate(year = .y))

sg_putting <- sg_putting %>% bind_rows() %>% as_tibble()

sg_off_tee <- map2(ball_striking_urls$sg_off_tee, 
                   ball_striking_urls$years, 
                   ~ scrape_pga(.x) %>% mutate(year = .y))

sg_off_tee <- sg_off_tee %>% bind_rows() %>% as_tibble()

sg_approach <- map2(ball_striking_urls$sg_app_green, 
                    ball_striking_urls$years, 
                    ~ scrape_pga(.x) %>% mutate(year = .y))

sg_approach <- sg_approach %>% bind_rows() %>% as_tibble()

avg_dist <- map2(ball_striking_urls$avg_driv_dist,
                 ball_striking_urls$years, 
                 ~ scrape_pga(.x) %>% mutate(year = .y))

avg_dist <- avg_dist %>% bind_rows() %>% as_tibble()

pct_total_money <- map2(ball_striking_urls$pct_total_money_won,
                        ball_striking_urls$years, 
                        ~ scrape_pga(.x) %>% mutate(year = .y))

pct_total_money <- pct_total_money %>% 
  bind_rows() %>% 
  as_tibble() %>%
  mutate(money = 
           gsub(pattern = ",",
                replace = "",
                x = gsub(pattern = "$",
                         replacement = "",
                         x = `OFFICIAL MONEY WON`,
                         fixed = TRUE)) %>%
           as.numeric())

sg_data <- sg_off_tee %>%
  select(year, `PLAYER NAME`) %>%
  bind_rows(sg_putting %>% select(year, `PLAYER NAME`)) %>%
  bind_rows(sg_around_green %>% select(year, `PLAYER NAME`)) %>%
  bind_rows(sg_approach %>% select(year, `PLAYER NAME`)) %>%
  bind_rows(avg_dist %>% select(year, `PLAYER NAME`)) %>%
  bind_rows(pct_total_money %>% select(year, `PLAYER NAME`)) %>%
  distinct() %>%
  filter(`PLAYER NAME` != "Richard Johnson")


sg_data2 <- sg_data %>%
  left_join(sg_off_tee %>%
              select(year, `PLAYER NAME`, SG_OFF_TEE = AVERAGE), 
            by = c("year", "PLAYER NAME")) %>%
  left_join(sg_approach %>%
              select(year, `PLAYER NAME`, SG_APPROACH = AVERAGE), 
            by = c("year", "PLAYER NAME")) %>%
  left_join(sg_putting %>%
              select(year, `PLAYER NAME`, SG_PUTTING = AVERAGE), 
            by = c("year", "PLAYER NAME")) %>%
  left_join(sg_around_green %>%
              select(year, `PLAYER NAME`, SG_AROUND_GREEN = AVERAGE), 
            by = c("year", "PLAYER NAME")) %>%
  left_join(avg_dist %>%
              select(year, `PLAYER NAME`, AVG_DIST = AVG.), 
            by = c("year", "PLAYER NAME")) %>%
  left_join(pct_total_money %>%
              select(year, `PLAYER NAME`, money,
                     EVENTS),
            by = c("year", "PLAYER NAME")) %>%
  na.omit() %>%
  filter(EVENTS >= 10) %>%
  as_tibble() %>%
  mutate(money_log = log(money)) %>%
  mutate_at(c("SG_OFF_TEE", "SG_APPROACH", "SG_PUTTING","SG_AROUND_GREEN"),
            funs("tenth" = . * 10))


fitmod <- function(data) lm(money_log ~ 
                              SG_OFF_TEE_tenth +
                              SG_APPROACH_tenth +
                              SG_PUTTING_tenth +
                              SG_AROUND_GREEN_tenth,
                            data = data)

sg_model <- sg_data2 %>%
  group_by(year) %>%
  nest() %>%
  mutate(model = map(data, fitmod))


sg_model2 <- sg_model %>%
  group_by(year)%>%
  mutate(model_coef = map(model, broom::tidy),
         model_glance = map(model, broom::glance))

sg_model2 %>%
  unnest(model_coef) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(year, exp(estimate) - 1, colour = term))+
  geom_line()+
  geom_point()+
  scale_y_continuous(labels = percent,
                     "% Increase in Earnings from a 0.1 Strokes Gained Improvement")

p1 <- sg_data2 %>%
  ggplot(aes(SG_APPROACH, SG_OFF_TEE))+
  stat_smooth(method = 'lm')+
  geom_point(alpha = 0.25, colour = 'black')+
  geom_point(data = sg_data2 %>% filter(grepl(pattern = "Koepka", 
                                              x = `PLAYER NAME`)),
             colour = 'blue', size = 3)+
  geom_point(data = sg_data2 %>% filter(grepl(pattern = "Tiger", 
                                              x = `PLAYER NAME`)),
             colour = 'red', size = 3)+
  geom_point(data = sg_data2 %>% filter(grepl(pattern = "Dustin Johnson", 
                                              x = `PLAYER NAME`)),
             colour = '#4DAF4A', size = 3)+
  geom_point(data = sg_data2 %>% 
               filter(`PLAYER NAME` %in% 
                        c("Dustin Johnson",
                          "Tiger Woods",
                          "Brooks Koepka")),
             size = 3,
             pch = 1)+
  xlab("Strokes Gained: Approach")+
  ylab("Strokes Gained: Off the Tee")+
  ggtitle("What is the relationship between strokes gained approach and strokes gained off the tee?",
          subtitle = c("Data from 2004-2019; Tiger: red dots, Brooks: blue, DJ: green"))



p2 <- sg_data2 %>%
  ggplot(aes(AVG_DIST, SG_APPROACH))+
  stat_smooth(method = 'lm')+
  geom_point(alpha = 0.25, colour = 'black')+
  geom_point(data = sg_data2 %>% filter(grepl(pattern = "Koepka", 
                                              x = `PLAYER NAME`)),
             colour = 'blue', size = 3)+
  geom_point(data = sg_data2 %>% filter(grepl(pattern = "Tiger", 
                                              x = `PLAYER NAME`)),
             colour = 'red', size = 3)+
  geom_point(data = sg_data2 %>% filter(grepl(pattern = "Dustin Johnson", 
                                              x = `PLAYER NAME`)),
             colour = '#4DAF4A', size = 3)+
  geom_point(data = sg_data2 %>% 
               filter(`PLAYER NAME` %in% 
                        c("Dustin Johnson",
                          "Tiger Woods",
                          "Brooks Koepka")),
             size = 3,
             pch = 1)
