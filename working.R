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
  right_join(perfect_data, by = c("air_date", "x_pos", "round"))

## Filtering out the daily doubles, and figuring out how many questions were 
## asked without DDs being in the mix. Next, joining this with 
## categories_expanded to get the full list of x, y positions. 
no_dd <- test %>% 
  filter(daily_double == "0") %>% 
  group_by(air_date, category, round) %>% 
  summarise(questions_asked = n()) %>% 
  right_join(categories_expanded)

## Joining test to no_dd for all the other information. Getting the total number
## of NA values (i.e. questions that could either be no asked OR a DD).
q_asked <- no_dd %>% 
  left_join(test) %>%
  mutate(questions_asked = as.numeric(questions_asked),
         na_count = 5 - questions_asked) %>% 
  replace_na(list(daily_double = "1")) %>% 
  mutate(daily_double = as.numeric(daily_double))

## Filtering by DD, and then weighting each not asked or DD question. Adding 
## This filter back to the q_asked df to have all the possible questions. 
## Replacing NAs with "0" so we have a full numeric column.
weighted_dd <- q_asked %>% 
  filter(daily_double == "1") %>% 
  mutate(daily_double = as.numeric(daily_double),
         dd_weighting = daily_double/na_count) %>% 
  right_join(q_asked) %>% 
  replace_na(list(dd_weighting = "0"))
  
  


