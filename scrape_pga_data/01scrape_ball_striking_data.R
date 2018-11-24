library(rvest)
library(glue)
library(tidyverse)

scrape_pga <- function(url){
  url %>%
    read_html() %>%
    html_table(fill = TRUE) %>%
    .[[2]]
}

ball_striking_urls <- data_frame(years = 2004:2018) %>%
  mutate(total_driving = glue("https://www.pgatour.com/stats/stat.129.{years}.html"),
         sg_tee_to_green = glue("https://www.pgatour.com/stats/stat.02674.{years}.html"),
         sg_off_tee = glue("https://www.pgatour.com/stats/stat.02567.{years}.html"),
         sg_app_green = glue("https://www.pgatour.com/stats/stat.02568.{years}.html"),
         gir_pct = glue("https://www.pgatour.com/stats/stat.103.{years}.html"))

webpage <- read_html("https://www.pgatour.com/stats/stat.02675.2018.html")

tbls <- html_table(webpage, fill = TRUE)

sg_tee_to_green <- ball_striking_urls$sg_tee_to_green %>%
  map(scrape_pga)

sg_tee_to_green %>%
  map_df(~.x %>%
           filter(`PLAYER NAME` == 'Charles Howell III'))
