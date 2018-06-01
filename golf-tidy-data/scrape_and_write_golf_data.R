library(tidyverse)
library(rvest)
# function that reads a WIAA webpage and returns the results from
# the tournament
full_scrape <- function(url){
  
  #read webpage and return tables
  all_tbls <- read_html(url) %>%
    html_nodes('table')
  # only select tables with scores
  all_tbls <- all_tbls[grepl(pattern = 'border="1"',
                             x = all_tbls)]
  # extract tables
  tbl_list <- lapply(all_tbls,
                     html_table,
                     header = TRUE)
  # convert column names to upper case
  tbl_list <- lapply(tbl_list, 
                     FUN = function(x) setNames(x, toupper(names(x))))
  # extract common names
  common_names <- Reduce(intersect, lapply(tbl_list, names))
  # bind tables together
  final_tbl <- do.call('rbind', 
                       lapply(tbl_list, FUN = function(x) x[common_names]))
  
  final_tbl
}
# all the web pages we need to scrape
course_links <- 
  c("https://www.wiaawi.org/Portals/0/PDF/Results/Golf_Boys/2011/maarathonsectional.htm",
    "https://www.wiaawi.org/Portals/0/PDF/Results/Golf_Boys/2012/marathonsectional.htm",
    "http://www.golfcoacheswi.org/EventScoring/WIAA/boysgolf2013venuescores.php?course=Marathon",
    "http://www.wiaawisc.org/Golf/boys2014venuescores.php?course=Marathon+HS",
    "http://www.wiaawisc.org/Golf/boysgolf2015venuescores.php?course=Marathon+High+School",
    "http://www.wiaawisc.org/Golf/boysgolf2016venuescores.php?course=Marathon+HS",
    "http://www.wiaagolf.org/ScoreEntry/boysgolf2017venuescores.php?course=Marathon+HS",
    "http://www.wiaagolf.org/ScoreEntry/boysgolf2018venuescores.php?course=Marathon")

# tournament information
tourn_type = c('sectionals', 
               'sectionals', 'regionals', 'regionals', 
               'sectionals','sectionals', 'regionals',
               'sectionals')
tourn_year <- 2011:2018

course_tables <- lapply(course_links, full_scrape)

course_tables <- lapply(1:length(course_tables), FUN = function(i){
  course_tables[[i]] %>%
    mutate(tourn_type = tourn_type[i],
           tourn_year = tourn_year[i])
})

common_names <- Reduce(intersect, lapply(course_tables, names))

course_tables <- do.call('rbind',
                         lapply(course_tables, 
                                FUN = function(x) x[common_names]))

# clean up results
course_tables_cleaned <- course_tables[!is.na(course_tables$`1`),] %>%
  filter(TOT != 'DQ') %>% # remove DQs
  filter(TOT != '172') %>% # remove dummy score
  mutate(NAME = paste0('A', row_number())) %>%
  mutate_at(c('OUT','IN','TOT'), as.numeric) 

# course data
pv_course <- data_frame(hole = as.character(1:18),
                        par = c(5, rep(4, 4), 3, rep(4, 2), 3,
                                4, 5, 4, 3, 5, rep(4,2), 3, 4),
                        OB = c(T, F, T, T, F, F, F, T, T,
                               T, F, T, F, F, F, F, F, T),
                        water = c(rep(T, 5), F, T, T, T,
                                  T, F, F, T, F, T, T, T, T),
                        side = rep(c('front', 'back'), each = 9))

# tidy up the data 
tidy_scores <- course_tables_cleaned %>%
  gather(hole, score, 
         -NAME, -YEAR, -OUT, -IN, -TOT,
         -tourn_type, -tourn_year) %>%
  mutate(score = as.numeric(score)) %>%
  inner_join(pv_course) %>%
  mutate(rel_to_par = score - par) 

# lower case names
names(course_tables_cleaned) <- tolower(names(course_tables_cleaned))
names(tidy_scores) <- tolower(names(tidy_scores))

write_csv(course_tables_cleaned, path = 'data/untidy_golf_scores.csv')
write_csv(tidy_scores, path = 'data/tidy_golf_scores.csv')

