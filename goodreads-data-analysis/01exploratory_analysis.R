library(tidyverse)
library(lubridate)

goodreads_data <- read_csv("goodreads_library_export.csv")


books_2020 <- goodreads_data %>%
  filter(year(`Date Read`) == 2020) %>%
  mutate(page_count = ifelse(grepl("Be a Player",
                                   Title),
                             256,
                             `Number of Pages`),
         rating_diff = `My Rating` - `Average Rating`,
         publish_year = coalesce(`Original Publication Year`,
                                  `Year Published`)) %>%
  rename(date_read = `Date Read`)


# timing of books
books_2020 %>%
  count(date_read) %>%
  mutate(cumulative_books = cumsum(n)) %>%
  ggplot(aes(date_read, cumulative_books))+
  geom_line()+
  geom_point()
# cumulative pages read
books_2020 %>%
  arrange(date_read) %>%
  mutate(cumulative_pages = cumsum(page_count)) %>%
  ggplot(aes(date_read, cumulative_pages))+
  geom_line()+
  geom_point()
# my rating vs average rating
books_2020 %>%
  ggplot(aes(reorder(Title, rating_diff), rating_diff))+
  geom_col()+
  coord_flip()


books_2020 %>%
  ggplot(aes(`My Rating`, rating_diff))+
  geom_point()


books_2020 %>%
  ggplot(aes(date_read, `Average Rating`))+geom_point()


books_2020 %>%
  ggplot(aes(date_read, rating_diff))+
  geom_point()


books_2020 %>%
  ggplot(aes(date_read, page_count))+
  geom_point()

summary_by_date <- books_2020 %>%
  group_by(date_read) %>%
  summarise(pages = sum(page_count)) %>%
  bind_rows(tibble(date_read = as.Date("2020-01-01"),
                   pages = 0)) %>%
  arrange(date_read) %>%
  mutate(previous_date = lag(date_read)) %>%
  mutate(days_since_last_book = as.numeric(difftime(
    date_read, previous_date, units = "days"
  )))
