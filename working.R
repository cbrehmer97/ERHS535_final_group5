library(tidyverse)
library(magrittr)
library(lubridate)

jeopardy_raw <- read_tsv("data/master_season1-35.tsv")

#initial clean
jeopardy_clean <- jeopardy_raw %>%
  filter(air_date > ymd("2001-11-26") & round != 3) %>% 
  mutate(daily_double = case_when(daily_double == "yes" ~ 1,
                                  daily_double == "no" ~ 0))

#check daily doubles in each episode
dd_episodes <- jeopardy_clean %>% 
  group_by(air_date) %>% 
  summarise(dd_count = sum(daily_double))

#Assign position
#Seperate by round
#assign y position based on value
#x position will be harder

test <- jeopardy_clean %>% 
  group_by(air_date) %>% 
  nest() %>% 
  mutate(categories = map(data, ~ unique(select(., category))),
         num_categories = map(categories, ~ nrow(.))) %>% 
  unnest(num_categories) %>% 
  filter(num_categories == 12) %>% 
  mutate(categories = map(categories, ~ rownames_to_column(., var = "x_pos")),
         data_keep = map2(categories, data, ~full_join(.x, .y, by = "category"))) %>% 
  select(air_date, data_keep) %>% 
  unnest(cols = data_keep) %>% 
  mutate(x_pos = case_when(x_pos == 7 | x_pos == 1 ~ 1,
                           x_pos == 8 | x_pos == 2 ~ 2,
                           x_pos == 9 | x_pos == 3 ~ 3,
                           x_pos == 10 | x_pos == 4 ~ 4,
                           x_pos == 11 | x_pos == 5 ~ 5,
                           x_pos == 12 | x_pos == 6 ~ 6))


?expand

test_name <- c("me", "you", "him", "her")

rep(test_name, times = 4)

df <- data.frame(a = c("me", "you", "him", "her")) 
df
df[rep(seq_len(nrow(df)), each = 2), ]


### Molly's added code (works to find the weightings)
library(dplyr)
library(tidyr)  

perfect_data <- test %>% 
  filter(daily_double == "0") %>% 
  group_by(air_date, round) %>% 
  expand(x_pos, value) %>% 
  mutate(x_pos = as.numeric(x_pos),
         y_pos = as.numeric(value),
         value = as.numeric(value)) %>% 
  group_by(round, x_pos, air_date) %>% 
  mutate(y_pos = c("1", "2", "3", "4", "5")) 

## Adding the categories to the 'perfect_data' that's been expanded to include
## all possible values
categories_expanded <- test %>% 
  group_by(air_date, round, category, x_pos) %>% 
  summarise(total = n()) %>% 
  right_join(perfect_data, by = c("air_date", "x_pos", "round")) %>% 
  select(-total)

## Filtering out the daily doubles, and figuring out how many questions were 
## asked without DDs being in the mix. 
no_dd <- test %>% 
  filter(daily_double == "0")
  
## 1 Figuring out how many questions were asked in a category.
## 2 Joining the dataset with the ALL combinations of x & y positions
## 3 Joining the dataset with the no_dd to gather the rest of the data
## 4 Finding the number of NAs. NA = either the spot of a DD OR a missing value.
## 5 Replacing all NAs in daily_double column with a '1'. 
q_asked <- no_dd %>% 
  group_by(air_date, category, round) %>% ## 1
  summarise(questions_asked = n()) %>%  ## 1
  right_join(categories_expanded) %>% ## 2
  left_join(no_dd) %>% ## 3
  mutate(questions_asked = as.numeric(questions_asked),
         na_count = 5 - questions_asked) %>% ## 4
  replace_na(list(daily_double = "1")) %>% ## 5
  mutate(daily_double = as.numeric(daily_double)) 

## 1 Mutating to create a 'weighted' col. This is daily_double (either 0 or 1) 
##   divided by the na_count (number of Nas of missing value OR DD spot)
## 2 Changing NA values (from taking 0/na_count) to "0".
weighted_dd <- q_asked %>% 
  mutate(daily_double = as.numeric(daily_double),
         dd_weighting = daily_double/na_count) %>% ## 1
  replace_na(list(dd_weighting = "0")) %>% ## 2
  select(-questions_asked, -notes, -na_count, -comments)

weighted_dd_clean <- weighted_dd[, c(1, 2, 3, 4, 6, 5, 7, 10, 8, 9)]  

## ONLY KNOWN daily double positions USE THIS FOR AVERAGE PT VALUE DD. 
known_dd <- test %>% 
  group_by(category, air_date, round) %>% 
  summarise(questions = n()) %>% 
  filter(questions == "5") %>% 
  left_join(test) %>% 
  ungroup() %>% 
  group_by(category, air_date, round) %>% 
  mutate(y_pos = c("1", "2", "3", "4", "5")) %>% 
  select(-questions)

library(ggplot2)
weighted_dd %>% 
  mutate(dd_weighting = as.numeric(dd_weighting)) %>% 
  group_by(x_pos, y_pos) %>% 
  summarise(number_of_doubles = sum(dd_weighting)) %>% 
  ggplot(aes(x = x_pos, y = y_pos, 
             fill = number_of_doubles )) +
  geom_tile(color = "White", size = 0.1) +
  geom_text(aes(label = round(number_of_doubles))) +
  scale_fill_gradient(high = "#132B43", 
                      low = "#56B1F7")


library(plotly)
weighted_dd %>% 
  mutate(dd_weighting = as.numeric(dd_weighting)) %>% 
  group_by(x_pos, y_pos) %>% 
  summarise(number_of_doubles = sum(dd_weighting)) %>% 
  plot_ly(
    x = ~ x_pos,
    y = ~ y_pos,
    z = ~ number_of_doubles,
    type = 'heatmap'
  )


