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
                                  `Year Published`))


# timing of books
books_2020 %>%
  count(`Date Read`) %>%
  mutate(cumulative_books = cumsum(n)) %>%
  ggplot(aes(`Date Read`, cumulative_books))+
  geom_line()+
  geom_point()
# cumulative pages read
books_2020 %>%
  arrange(`Date Read`) %>%
  mutate(cumulative_pages = cumsum(page_count)) %>%
  ggplot(aes(`Date Read`, cumulative_pages))+
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
  ggplot(aes(`Date Read`, `Average Rating`))+geom_point()


books_2020 %>%
  ggplot(aes(`Date Read`, rating_diff))+
  geom_point()


books_2020 %>%
  ggplot(aes(`Date Read`, page_count))+
  geom_point()
