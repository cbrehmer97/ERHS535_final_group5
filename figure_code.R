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
