
# Data Cleanup

library(rvest)
library(dplyr)
library(tidyr)
library(tidyverse)


link <- "https://en.wikipedia.org/wiki/List_of_highest-grossing_concert_tours_by_women"

page <- read_html(link)
page


Concert_tours <- html_element(page, "table.sortable") %>% html_table()

head(Concert_tours)
tail(Concert_tours)

str(Concert_tours)

Concert_tours <- Concert_tours %>% select(-Rank, -Ref.)

# -----------------------------------------------------

# write.csv(Concert_tours, "Concert_Tours.csv", row.names = FALSE)

df <- read.csv("C:/Users/jakeb/OneDrive/Documents/R Coding/concert_Tours.csv", 
               header = TRUE)

df <- separate(df, col = Peak, into = c('Peak', NA))

df <- separate(df, col = All.timepeak, into = c('All Time Peak', NA))

colnames(df) <- c('Peak','All time peak','Actual Gross',
                  'Adjusted Gross (2024 USD)','Artist','Tour title','Year(s)',
                  'Shows','Avg. Gross')

df <- separate(df, col = c('Tour title'), into = c('Tour title', NA), sep = "‡")

df <- separate(df, col = c('Tour title'), into = c('Tour title', NA), sep = "†")

df$Peak <- as.numeric(df$Peak)

df$`All time peak` <- as.numeric(df$`All time peak`)

df[is.na(df)] <- 0

summary(df)

df %>% group_by(Artist, `Actual Gross`) %>% summarise(Shows = Shows) %>% arrange(desc(Shows))

df %>%
  ggplot(aes(x = (Shows), y = (`Actual Gross`))) +
  geom_point(position = "jitter") +
  labs(y = "Gross", x = "Number of Shows")









