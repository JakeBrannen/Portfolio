
# Web scraping

library(rvest)
library(dplyr)
library(tidyr)
library(tidyverse)

# ------------------------------------------------

# Data Gathering


link <- "https://en.wikipedia.org/wiki/List_of_Formula_One_drivers"

page <- read_html(link)
page

drivers_F1 <- html_element(page, "table.sortable") %>% html_table()

head(drivers_F1)
tail(drivers_F1)

str(drivers_F1)

# -----------------------------------------------

# Data Cleanup

drivers_F1 <- drivers_F1[c(1:4, 7:9)] # select variables

drivers_F1 <- drivers_F1[-nrow(drivers_F1), ]

drivers_F1$`Drivers' Championships` <- substr(drivers_F1$`Drivers' Championships`, start = 1, stop = 1)

# -------------------------------------------------

# Download Dataset

# write.csv(drivers_F1, "F1_drivers.csv", row.names = FALSE)

# NOTE: You want to download the dataset as a csv so that not all data is classified
# as character by default

# -------------------------------------------------

# Data Analysis

drivers_F1 %>% group_by(Nationality) %>% 
  summarise(championship_country = sum(as.double(`Drivers' Championships`))) %>% 
  arrange(desc(championship_country))

drivers_F1 %>%
  group_by(`Driver name`) %>%
  summarise(championship_pilot = sum(as.double(`Drivers' Championships`))) %>%
  arrange(desc(championship_pilot))

drivers_F1 %>%
  filter(`Pole positions` > 1) %>%
  ggplot(aes(x = as.double(`Pole positions`), y = as.double(`Drivers' Championships`))) +
  geom_point(position = "jitter") +
  labs(y = "Championships won", x = "Pole positions") +
  theme_minimal()

# --------------------------------------------------

df <- read.csv("C:/Users/jakeb/OneDrive/Documents/R Coding/F1_drivers.csv", 
               header = TRUE)















