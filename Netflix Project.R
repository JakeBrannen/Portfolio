
# Netlflix Project

library(mice) # Use for NA's
library(caret)
library(yardstick)
library(tidyverse)
library(GGally)
library(reshape2)
library(plotly)

netflix <- read.csv("C:/Users/jakeb/OneDrive/Documents/R Coding/netflix/netflix_titles.csv", 
               header = TRUE)

# In case you want to have column in different dataset

# shows <- subset(netflix, type == "TV Show")
# movies <- subset(netflix, type == "Movie")

# -----------------------------------------

# Display how many more movies there are than shows on Netflix

netflix %>% filter(type == 'Movie' | type == 'TV Show') %>% 
  ggplot(aes(type, fill = type)) + geom_bar()

# --------------------------------

# Best time to release new content for Netflix
# (When is less content released on a month by month basis)


# Clean and extract date info
netflix_date <- netflix %>%
  filter(!is.na(date_added)) %>%
  mutate(
    date_added = str_trim(date_added),
    year = str_extract(date_added, "\\d{4}$"),
    month = word(date_added, 1)
  )

# Define month order reversed
month_order <- rev(month.name)

# Count shows by year and month
df <- netflix_date %>%
  count(year, month) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0) %>%
  slice(match(month_order, month)) %>%
  column_to_rownames("month")

# Convert to matrix for heatmap
df_matrix <- as.matrix(df)

df_melt <- melt(df_matrix)
colnames(df_melt) <- c("Month", "Year", "Count")

ggplot(df_melt, aes(x = Year, y = Month, fill = Count)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Netflix Contents Update",
    x = NULL, y = NULL, fill = "Count"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8))

# ------------------------------------------

# Visualize content by its film rating 

ggplot(data = netflix, aes(rating, fill = rating)) +
  geom_bar() +
  scale_x_discrete(limits = names(sort(table(netflix$rating), decreasing = TRUE)[1:15])) + 
  theme(panel.grid.major = element_line(color = "grey80"), panel.grid.minor = element_blank())

# -------------------------------------------

netflix %>% select(rating, type) %>% filter(!is.na(rating)) %>% 
  mutate(rating = fct_lump(rating, 5)) %>% group_by(rating, type) %>%
  summarise(Count = n()) %>% arrange(Count) %>% 
  plot_ly(x = ~ type , y = ~ Count, type = "bar", color = ~ rating, 
          text = ~ Count, textposition = 'outside', 
          textfont = list(color = '#000000', size = 12)) %>%
  layout(yaxis = list(categoryorder = "array", categoryarray = ~ Count)) %>%
  layout(title = "Rating by Type", yaxis = list(title = "Type"),
         xaxis = list(title = "Count"), legend = list(title = list(text = '<b> Rating </b>')))
# -----------------------------------------------


movies <- netflix %>% select(country, type, duration, rating, title) %>%
  filter(type == "Movie") %>% drop_na() %>% mutate(duration_min = parse_number(duration))

tv_show <- netflix %>% select(country, type, duration, rating, title) %>%
  filter(type == "TV Show") %>% drop_na() %>% mutate(duration_season = parse_number(duration))

movies %>% plot_ly(x = ~ duration_min, type = "histogram",
    nbinsx = 40, marker = list(color = "drakblue",line = list(color = "black", width = 1))) %>% 
  layout(title = "Duration distrbution", yaxis = list(title = "Count", zeroline = FALSE), 
         xaxis = list(title = "Duration (min)", zeroline = FALSE)) 

# -----------------------------------------------

countries<-netflix%>% select(country, type, title, listed_in)

sum(is.na(countries$country))/nrow(countries)
#country NA's rate: 1% => delete them(NA value)

countries<-countries %>% filter(!is.na(country))

max(str_count(countries$country, ','))
#max = 11 ',' => maximum = 12 countries

# split the combined countries into single one
ctr<-countries%>% separate(country, into = c('a','b','c','d','e','f','g','h','i','j','k','l')
           ,", ", convert = TRUE)

ctr<-ctr[,1:12]

ctr_list<-ctr%>% unlist()

ctr_tibble<-tibble(country_name=ctr_list)


ctr <- ctr_tibble %>% group_by(country_name) %>% count()%>% filter(!is.na(country_name))

ctr <- ctr[-1,-2]

max(str_count(countries$listed_in, ','))

List_in <- countries %>% select(listed_in) %>% separate(listed_in, into = c('a','b','c'),", ", convert = TRUE)

List_in <- List_in %>% unlist()

list_in<- tibble(list_in=List_in)

list_in %>% group_by(list_in) %>% count() %>% filter(!is.na(list_in) && n>=100) %>% 
  ggplot(aes(n, reorder(list_in, fun=median, n), fill = n>1500)) + 
  geom_histogram(stat = 'identity', show.legend = F) + 
  labs(x='Numbers of type in movies on Netflix', y='Types',title='Most popular types on Netflix')

# --------------------------------------------






















