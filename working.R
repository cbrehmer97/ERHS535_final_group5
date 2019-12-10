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

#Function w mapping
test1 <- jeopardy_clean %>% 
  mutate(dd_bet = case_when(daily_double == 1 ~ value,
                            daily_double == 0 ~ 0),
         value = case_when(daily_double == 1 ~ 5,
                           daily_double == 0 ~ value)) %>% 
  group_by(air_date) %>% 
  nest() %>% 
  mutate(categories_unique = map(data, ~ unique(select(., category))),
         categories_asked = map(data, ~ select(., round, category, value, dd_bet, daily_double)),
         num_categories = map(categories_unique, ~ nrow(.))) %>% 
  unnest(num_categories) %>% 
  filter(num_categories == 12) %>% 
  mutate(categories_unique = map(categories_unique, ~ rownames_to_column(., var = "x_pos")),
         categories_pos = map2(categories_unique, categories_asked, ~full_join(.x, .y)),
         categories_pos = map(categories_pos, ~ group_by(., round)),
         categories_pos = map(categories_pos, ~ expand(., x_pos, value)))


test2 <- test1 %>% 
  mutate(cat_dd = map(categories_asked, ~ filter(., daily_double == 1)),
         cat_dd = map(cat_dd, ~ select(., category)),
         categories_test = map2(categories_asked, cat_dd, ~ right_join(.x, .y, by = "category")),
         categories_test = map(categories_test, ~ group_by(., category)),
         categories_test = map(categories_test, ~ mutate(., count = n(),
                                                         weight = if_else(count == 5, 1, 1/(6-count)))),
         test_join = map2(categories_unique, categories_pos, ~ full_join(.x, .y, by = c("x_pos"))),
         categories_expanded = map2(test_join, categories_asked, ~ full_join(.x, .y, by = c("round", "category", "value"))),
         categories_expanded = map2(categories_expanded, cat_dd, ~ right_join(.x, .y, by = "category")),
         categories_test = map(categories_test, ~ select(., category, weight)),
         categories_test = map(categories_test, ~ distinct(.)),
         categories_expanded = map2(categories_expanded, categories_test,
                                    ~ full_join(.x, .y, by = "category")),
         categories_expanded = map(categories_expanded, ~ mutate(., daily_double = case_when(is.na(daily_double) == TRUE ~ weight,
                                                                                             is.na(daily_double) == FALSE ~ daily_double))),
         categories_expanded = map(categories_expanded, ~ filter(., value != 5)),
         test_join = map(test_join, ~ filter(., value != 5)),
         test_join = map(test_join, ~ group_by(., category)),
         test_join = map(test_join, ~ mutate(., y_pos = c("1", "2", "3", "4", "5"))),
         data_keep = map2(test_join, categories_expanded, ~ full_join(.x, .y, by = c("x_pos", "category", "round", "value"))))

#Contains final information needed
test_final <- test2 %>% 
  select(air_date, data_keep) %>% 
  unnest(cols = data_keep) %>% 
  filter(daily_double != 0) %>% 
  select(air_date, category, round, x_pos, y_pos, value, weight) %>% 
  mutate(x_pos = case_when(x_pos == 7 | x_pos == 1 ~ 1,
                           x_pos == 8 | x_pos == 2 ~ 2,
                           x_pos == 9 | x_pos == 3 ~ 3,
                           x_pos == 10 | x_pos == 4 ~ 4,
                           x_pos == 11 | x_pos == 5 ~ 5,
                           x_pos == 12 | x_pos == 6 ~ 6))

### Begining of Molly's code
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

category_with_dd <- test %>% 
  filter(daily_double == "1") %>% 
  group_by(air_date, category, round) %>% 
  summarise(total = n()) %>% 
  select(-total) %>% 
  left_join(no_dd)

only_dd_in_cat <- test %>% 
  group_by(air_date, category, round) %>% 
  summarise(total = n()) %>% 
  filter(total == "1") %>% 
  left_join(test) %>% 
  filter(daily_double == "1") %>% 
  slice(rep(1:n(), each = 5)) %>% 
  group_by(air_date, category) %>% 
  mutate(value = ifelse(round == "1", c("200", "400", "600", "800", "1000"),
                        c("400", "800", "1200", "1600", "2000")),
         value = as.numeric(value)) %>% 
  group_by(air_date, category, round) %>% 
  mutate(y_pos = c("1", "2", "3", "4", "5"),
         dd_weighting = 1/5) %>% 
  select(-total)

## 1 Figuring out how many questions were asked in a category.
## 2 Joining the dataset with the ALL combinations of x & y positions expanded
## 3 Joining the dataset with category_with_dd for rest of the data.
## 4 Finding the number of NAs. NA = either the spot of a DD OR a missing value.
## 5 Filtering out the NAs (these are categories where there isn't a DD)
## 6 Making all the NA spots "1" to mark category w/ DD.

## 1 Mutating to create a 'weighted' col. This is daily_double (either 0 or 1) 
##   divided by the na_count (number of Nas of missing value OR DD spot)
## 2 Changing NA values (from taking 0/na_count) to "0".
weighted_dd <- category_with_dd %>% 
  group_by(air_date, category, round) %>% ## 1
  summarise(questions_asked = n()) %>%  ## 1
  right_join(categories_expanded) %>% ## 2
  left_join(category_with_dd) %>% ## 3
  mutate(questions_asked = as.numeric(questions_asked),
         na_count = 5 - questions_asked) %>% ## 4
  mutate(daily_double = as.numeric(daily_double)) %>% 
  filter(!is.na(questions_asked)) %>% ## 5
  replace_na(list(daily_double = 1)) %>%  ##6
  mutate(dd_weighting = daily_double/na_count) %>% ## 1
  replace_na(list(dd_weighting = 0)) %>% ## 2
  full_join(only_dd_in_cat) %>% 
  select(-questions_asked, -na_count) 


## ONLY KNOWN daily double positions USE THIS FOR AVERAGE PT VALUE DD. 
#known_dd <- test %>% 
#group_by(category, air_date, round) %>% 
#summarise(questions = n()) %>% 
#filter(questions == "5") %>% 
#left_join(test) %>% 
#ungroup() %>% 
#group_by(category, air_date, round) %>% 
#mutate(y_pos = c("1", "2", "3", "4", "5")) %>% 
#select(-questions)

dd <- test %>% 
  filter(daily_double == "1") %>% 
  right_join(weighted_dd, by = c("air_date", "category", "daily_double", 
                                 "round", "x_pos")) %>% 
  replace_na(list(comments.x = 0)) %>% 
  replace_na(list(answer.x = 0)) %>% 
  replace_na(list(question.x = 0)) %>% 
  replace_na(list(notes.x = 0)) %>% 
  mutate(comments.x = ifelse(comments.x == 0, comments.y, comments.x),
         answer.x = ifelse(answer.x == 0, answer.y, answer.x),
         question.x = ifelse(question.x == 0, question.y, question.x),
         notes.x = ifelse(notes.x == 0, notes.y, notes.x)) %>% 
  rename("comments" = "comments.x",
         "answer" = "answer.x",
         "question" = "question.x",
         "notes" = "notes.x",
         "dd_value" = "value.x",
         "value" = "value.y") %>% 
  select(-comments.y, -answer.y, -question.y, -notes.y)

dd <- dd[, c(1, 2, 12, 11, 5, 6, 13, 3, 4, 7, 8, 9, 10)]


sum(test$daily_double)
sum(weighted_dd$dd_weighting)

library(ggplot2)
dd %>% 
  #filter(round == "2") %>% 
  mutate(dd_weighting = as.numeric(dd_weighting)) %>% 
  group_by(x_pos, y_pos) %>% 
  summarise(number_of_doubles = sum(dd_weighting)) %>% 
  ggplot(aes(x = x_pos, y = y_pos, 
             fill = number_of_doubles )) +
  geom_tile(color = "White", size = 0.1) +
  geom_text(aes(label = round(number_of_doubles))) +
  scale_fill_gradient(high = "#132B43", 
                      low = "#56B1F7")

#Begining of Nikki's code
library(plotly)

#Create heat maps by round
#Round 1
dd_1 <- dd %>%
  filter(round == "1")

dd_1 %>% 
  mutate(dd_weighting = as.numeric(dd_weighting)) %>% 
  mutate(y_pos = factor(y_pos), 
         y_pos = 
           factor(y_pos, 
                  levels = rev(levels(y_pos)))) %>%
  group_by(x_pos, y_pos) %>% 
  summarise(number_of_doubles = sum(dd_weighting)) %>% 
  plot_ly(
    x = ~ x_pos,
    y = ~ y_pos,
    z = ~ number_of_doubles,
    colors = "Blues",
    type = 'heatmap',
    reversescale = FALSE
    ) %>%
  layout(title = "Number of Daily Doubles, Round 1", 
         xaxis = list(title = ""), 
         yaxis = list(title = ""))

#Round 2
dd_2 <- dd %>%
  filter(round == "2")

dd_2 %>% 
  mutate(dd_weighting = as.numeric(dd_weighting)) %>% 
  mutate(y_pos = factor(y_pos), 
         y_pos = 
           factor(y_pos, 
                  levels = rev(levels(y_pos)))) %>%
  group_by(x_pos, y_pos) %>% 
  summarise(number_of_doubles = sum(dd_weighting)) %>% 
  plot_ly(
    x = ~ x_pos,
    y = ~ y_pos,
    z = ~ number_of_doubles,
    type = 'heatmap',
    reversescale = TRUE
    ) %>%
  layout(title = "Number of Daily Doubles, Round 2", 
         xaxis = list(title = ""), 
         yaxis = list(title = ""))

RColorBrewer::brewer.pal.info

#End Nikki's code

year_plot <- dd %>% 
  mutate(daily_double = as.numeric(daily_double),
         year = year(air_date)) %>% 
  group_by(x_pos, y_pos, year) %>%
  summarise(number_of_doubles = sum(dd_weighting)) %>% 
  ungroup() %>% 
  mutate(y_pos = as.numeric(y_pos),
         number_of_doubles = as.numeric(number_of_doubles)) %>% 
  mutate(y_pos = factor(y_pos), 
         y_pos = 
           factor(y_pos, 
                  levels = rev(levels(y_pos)))) %>% 
  group_by(year) %>% 
  nest()

year_plot %>% 
  unnest() %>% 
  plot_ly(
    x = ~ x_pos,
    y = ~ y_pos,
    z = ~ number_of_doubles,
    frame = ~ year, 
    text = ~ number_of_doubles,
    type = 'heatmap',
    reversescale = TRUE
  ) 

