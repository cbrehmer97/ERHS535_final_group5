library(tidyverse)
library(magrittr)
library(lubridate)

jeopardy_raw <- read_tsv("data/master_season1-35.tsv")

#initial clean
jeopardy_clean <- jeopardy_raw %>%
  filter(air_date > ymd("2001-11-26") & round != 3) %>% 
  mutate(daily_double = case_when(daily_double == "yes" ~ 1,
                                  daily_double == "no" ~ 0))

#Function w mapping
jeopardy_final <- jeopardy_clean %>% 
  #Change "value" of daily double to be 5 to deal with issues expanding later on
  mutate(dd_bet = case_when(daily_double == 1 ~ value,
                            daily_double == 0 ~ 0),
         value = case_when(daily_double == 1 ~ 5,
                           daily_double == 0 ~ value)) %>% 
  #Nest data by air date
  group_by(air_date) %>% 
  nest() %>% 
  #Determine number of categories that appaeared in each episode and filter to only episodes with all catagories asked
  mutate(categories_unique = map(data, ~ unique(select(., category))),
         categories_asked = map(data, ~ select(., round, category, value, dd_bet, daily_double)),
         num_categories = map(categories_unique, ~ nrow(.))) %>% 
  unnest(num_categories) %>% 
  filter(num_categories == 12) %>% 
  #Assigns x position to each category and creates "perfect" data frame
  mutate(categories_unique = map(categories_unique, ~ rownames_to_column(., var = "x_pos")),
         perfect_pos = map2(categories_unique, categories_asked, ~full_join(.x, .y)),
         perfect_pos = map(perfect_pos, ~ group_by(., round)),
         perfect_pos = map(perfect_pos, ~ expand(., x_pos, value))) %>% 
  mutate(#Determine the categories where the DD was asked
         categories_dd = map(categories_asked, ~ filter(., daily_double == 1)),
         categories_dd = map(categories_dd, ~ select(., category)),
         #Filter each episode data to only contain information about category where DD was asked
         dd_weight = map2(categories_asked, categories_dd, ~ right_join(.x, .y, by = "category")),
         #Weight the chance that DD was asked at y position based on number of questions asked in each category
         dd_weight = map(dd_weight, ~ group_by(., category)),
         dd_weight = map(dd_weight, ~ mutate(., count = n(),
                                                         weight = if_else(count == 5, 1, 1/(6-count)))),
         #Full join to get x position of each category
         categories_perfect = map2(categories_unique, perfect_pos, ~ full_join(.x, .y, by = c("x_pos"))),
         #Make perfect data frame for daily double categories
         dd_perfect = map2(categories_perfect, categories_asked, ~ full_join(.x, .y, by = c("round", "category", "value"))),
         dd_perfect = map2(dd_perfect, categories_dd, ~ right_join(.x, .y, by = "category")),
         #Filter down to what the weight should be for the questions in the DD category
         dd_weight = map(dd_weight, ~ select(., category, weight)),
         dd_weight = map(dd_weight, ~ distinct(.)),
         #Add weight to places where DD could have been asked
         dd_perfect = map2(dd_perfect, dd_weight,
                                    ~ full_join(.x, .y, by = "category")),
         dd_perfect = map(dd_perfect, ~ mutate(., daily_double = case_when(is.na(daily_double) == TRUE ~ weight,
                                                                                             is.na(daily_double) == FALSE ~ daily_double))),
         #Remove expanded DD value of 5
         dd_perfect = map(dd_perfect, ~ filter(., value != 5)),
         categories_perfect = map(categories_perfect, ~ filter(., value != 5)),
         #Add y position
         categories_perfect = map(categories_perfect, ~ group_by(., category)),
         categories_perfect = map(categories_perfect, ~ mutate(., y_pos = c("1", "2", "3", "4", "5"))),
         #Combine perfect daily double and all categories into one final dataframe
         data_keep = map2(categories_perfect, dd_perfect, ~ full_join(.x, .y, by = c("x_pos", "category", "round", "value")))) %>% 
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

###Start of Nikki's code
library(plotly)

#Create heat maps by round
#Round 1

dd_1 <- dd %>% 
  filter(round == "1") %>%
  mutate(categories_heatmap = case_when(x_pos == "1" ~ "Category 1",
                                        x_pos == "2" ~ "Category 2",
                                        x_pos == "3" ~ "Category 3",
                                        x_pos == "4" ~ "Category 4",
                                        x_pos == "5" ~ "Category 5",
                                        x_pos == "6" ~ "Category 6")) %>%
  mutate(dd_weighting = as.numeric(dd_weighting)) %>% 
  group_by(categories_heatmap, value) %>% 
  summarise(number_of_doubles = sum(dd_weighting)) %>% 
  mutate(Percent = round(number_of_doubles/2476*100, digits = 2)) %>%
  plot_ly(
    x = ~ categories_heatmap,
    y = ~ value,
    z = ~ Percent,
    hovertemplate = paste("Daily Double Percent: %{z:,}%<br>",
                          "<extra></extra>"),
    colors = "Blues",
    type = 'heatmap'
    ) %>%
  layout(title = list(text = ""), 
         xaxis = list(title = "", 
                      side = 'top',
                      tickangle = -45), 
         yaxis = list(title = "", 
                      range = c(1000, 800, 600, 400, 200)))
dd_1

#Round 2
dd_2 <- dd %>%
  filter(round == "2") %>%
  mutate(categories_heatmap = case_when(x_pos == "1" ~ "Category 1",
                                        x_pos == "2" ~ "Category 2",
                                        x_pos == "3" ~ "Category 3",
                                        x_pos == "4" ~ "Category 4",
                                        x_pos == "5" ~ "Category 5",
                                        x_pos == "6" ~ "Category 6")) %>%
  mutate(dd_weighting = as.numeric(dd_weighting)) %>% 
  group_by(categories_heatmap, value) %>% 
  summarise(number_of_doubles = sum(dd_weighting)) %>% 
  mutate(Percent = round(number_of_doubles/4887*100, digits = 2)) %>%
  plot_ly(
    x = ~ categories_heatmap,
    y = ~ value,
    z = ~ Percent,
    hovertemplate = paste("Daily Double Percent: %{z:,}%<br>",
                          "<extra></extra>"),
    colors = "Blues",
    type = 'heatmap'
    ) %>%
  layout(title = list(text = ""), 
         xaxis = list(title = "", 
                      side = 'top',
                      tickangle = -45), 
         yaxis = list(title = "", 
                      range = c(2000, 1600, 1200, 800, 400), 
                      dtick = 400, tick0 = 2000))
dd_2

###End Nikki's code

###Begining of Molly's code
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
