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


### Molly's added code
library(dplyr)
library(tidyr)  

## Using expand to create dataframe with all possible values. 
perfect_data <- test %>% 
  filter(daily_double == "0") %>% 
  group_by(air_date, round) %>% 
  expand(x_pos, value) %>% 
  mutate(x_pos = as.numeric(x_pos),
         y_pos = as.numeric(value),
         value = as.numeric(value)) %>% 
  group_by(round, x_pos, air_date) %>% 
  mutate(y_pos = c("1", "2", "3", "4", "5")) 

# Filtering to include ONLY 1:5 questions asked (really want to locate DD in these)
molly_test <- test %>% 
  group_by(air_date, round, x_pos) %>% 
  summarise(total = n()) %>% 
  filter(total == "5") %>% 
  left_join(test, by = c("round", "air_date", "x_pos")) %>% 
  group_by(air_date, round, x_pos) %>% 
  mutate(y_pos = c("1", "2", "3", "4", "5")) %>% 
  select(-total) %>% 
  ungroup()

add_missing_values <- molly_test %>% 
  right_join(perfect_data, by = c("air_date", "x_pos", "round", "y_pos"))

all <- test %>% 
  left_join(add_missing_values)

na_values <- all %>% 
  filter(is.na(y_pos)) %>% 
  left_join(perfect_data, by = c("air_date", "x_pos", "round", "value")) %>% 
  select(-value.x, -y_pos.x, -value.y)

dd_positions <- all %>% 
  full_join(na_values) %>% 
  mutate(y_pos = coalesce(y_pos, y_pos.y)) %>% 
  select(-value.x, -value.y, -y_pos.y) #%>% 
  #right_join(perfect_data)
  
weighting <- dd_positions %>% 
  group_by(air_date, category, round) %>% 
  summarise(total = n()) %>% 
  left_join(dd_positions) #%>% 
  #filter(is.na(y_pos))


daily_double_positions <- dd_positions %>% 
  group_by(x_pos, y_pos) %>%
  summarise(number_of_doubles = sum(daily_double))
  #mutate(value_number = as.numeric(value_number),
         #number_of_doubles = as.numeric(number_of_doubles))
