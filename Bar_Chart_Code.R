library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

HDI_data <- read_excel("HDR23-24_Statistical_Annex_HDI_Trends_Table.xlsx")
colnames(HDI_data)
HDI_data <- HDI_data %>%
  rename(country = `Table 2. Human Development Index trends, 1990-2022`)
clean_HDI_data <- HDI_data %>% 
  select(...1, country, ...3, ...5, ...7, ...9, ...11, ...13, ...15, ...17, ...27) %>% 
  rename(HDI_rank = ...1, '1990' = ...3, '2000' = ...5, '2010' = ...7, '2015' = ...9, '2019' = ...11, '2020' = ...13, '2021' = ...15, '2022' = ...17, '1990-2022' = ...27)

glimpse(clean_HDI_data)
colnames(clean_HDI_data)
clean_HDI_data <- clean_HDI_data %>% 
  filter(!is.na(HDI_rank), !is.na(country), !is.na(`1990`), !is.na(`2000`), !is.na(`2010`), !is.na(`2015`), !is.na(`2019`), !is.na(`2020`), !is.na(`2021`), !is.na(`2022`), !is.na(`1990-2022`))
brics_countries <- c("Brazil", "Russian Federation", "India", "China", "Egypt", "Ethiopia", "Iran (Islamic Republic of)", "United Arab Emirates", "South Africa")

graph_data <- clean_HDI_data %>% 
  select(country, `1990-2022`) %>% 
  filter(country %in% brics_countries)
graph_data <- graph_data %>%
  mutate(`1990-2022` = as.numeric(`1990-2022`))
graph_data <- graph_data %>%
  filter(!is.na(`1990-2022`))
colors <- c("#334680", "#674a8f", "#984992", "#c54888", "#e94775", "#ff635a", "#ff8239", "#ffa600")
country_colors <- c("China" = "#334680", "India" = "#674a8f", "United Arab Emirates" = "#984992", "Egypt" = "#c54888", "Iran (Islamic Republic of)" = "#e94775", "Brazil" = "#ff655a", "South Africa" = "#ff8239", "Russian Federation" = "#ffa600")

graph_data %>%
  ggplot() +
  geom_col(aes(y = reorder(country, `1990-2022`), x = `1990-2022`, fill = country), width = 0.85) +
  scale_fill_manual(values = country_colors) +
  geom_text(aes(x = 0.05, y = reorder(country, `1990-2022`), label = country), size = 5, hjust = 0, color = "white") +
  geom_text(aes(x = `1990-2022` + 0.05, y = reorder(country, `1990-2022`), label = round(`1990-2022`, 2)), size = 5, hjust = 0) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("China has the Highest Average Annual HDI Growth Amongst BRICS Countries (1990-2022)") +
  labs(
    subtitle = "Human Development Index (HDI): A composite index measuring average achievement in three basic dimensions of human development — a long and healthy life,\nknowledge, and a decent standard of living.",
    caption = "Data source: United Nations Development Programme"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    plot.caption = element_text(size = 10)
  )

