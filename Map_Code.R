library(dplyr)
library(tidyverse)
library(sf)
library(viridis)
library(readxl)
library(countrycode)

hdi_2022 <- clean_HDI_data %>%
  select(country, HDI_rank, `2022`)
hdi_2022 <- hdi_2022[-1,]
glimpse(hdi_2022)

hdi_2022 <- hdi_2022 %>%
  mutate(`2022` = as.numeric(`2022`)) %>% 
  mutate(HDI_rank = as.numeric(HDI_rank))
hdi_2022 <- hdi_2022 %>% 
  mutate(category = case_when(HDI_rank >= 1 & HDI_rank <= 69 ~ "Very High Human Development",
                              HDI_rank >= 70 & HDI_rank <= 118 ~ "High Human Development",
                              HDI_rank >= 119 & HDI_rank <= 159 ~ "Medium Human Development",
                              HDI_rank >= 160 & HDI_rank <= 193 ~ "Low Human Development"))
hdi_2022$category <- factor(hdi_2022$category, levels = c("Very High Human Development",
                                                          "High Human Development",
                                                          "Medium Human Development",
                                                          "Low Human Development",
                                                          "NA"))

iso_codes <- countrycode(hdi_2022$country, "country.name", "iso3c")
hdi_2022$ISO_A3 <- iso_codes
glimpse(hdi_2022)

map_world <- st_read("WB_countries_Admin0_10m")
names(map_world)
st_crs(map_world)
ggplot() +
  geom_sf(data = map_world)
map_world_hdi <- left_join(map_world, hdi_2022, by = c("ISO_A3" = "ISO_A3"))

ggplot() +
  geom_sf(data = map_world_hdi, aes(fill = category)) +
  scale_fill_manual(values = c("Very High Human Development" = "#08306b",
                               "High Human Development" = "#2171b5",
                               "Medium Human Development" = "#6baed6",
                               "Low Human Development" = "#c6dbef",
                               "NA" = "#f0f0f0")) +
  labs(fill = "HDI Category", title = "World Human Development Index (HDI) in 2022") +
  theme_bw()
